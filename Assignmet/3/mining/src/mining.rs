extern crate chashmap;
extern crate itertools;
extern crate rayon;
extern crate reqwest;
use error_chain::error_chain;
use itertools::Itertools;
use rayon::iter::*;
use std::fs::File;
use std::io::copy;
use std::io::Read;
use std::path::PathBuf;

// This was done with help from Parm

error_chain! {
    foreign_links {
        Io(std::io::Error);
        HttpRequest(reqwest::Error);
    }
}

fn merge<'a>(left: Vec<(&'a str, f64)>, right: Vec<(&'a str, f64)>) -> Vec<(&'a str, f64)> {
    let mut vec = Vec::new();
    let (mut left, mut right) = (left.as_slice(), right.as_slice());
    while !(left.is_empty() || right.is_empty()) {
        if left[0].1 > right[0].1 {
            vec.push(right[0].clone());
            right = &right[1..];
        } else {
            vec.push(left[0].clone());
            left = &left[1..];
        }
    }

    if !left.is_empty() {
        vec.extend(left);
    } else {
        vec.extend(right);
    }
    return vec;
}

fn merge_sort(vec: Vec<(&str, f64)>) -> Vec<(&str, f64)> {
    return vec
        .par_iter()
        .cloned()
        .map(|val| vec![val])
        .reduce(|| vec![], |left, right| merge(left, right));
}

fn parser(line: String) -> (String, i64) {
    let vals: Vec<&str> = line[..75].split(',').take(15).collect();
    let time = match vals[14].trim().parse::<i64>() {
        Ok(val) => val,
        Err(_err) => 0i64,
    };
    return (String::from(vals[8].trim()), time);
}

fn check_ontime<'a>(airline: &'a str, times: &'a Vec<i64>) -> (&'a str, f64) {
    let size = times.len() as i64;
    let count: i64 = times
        .to_vec()
        .par_iter()
        .map(|state| if state.clone() > 0 { 0 } else { 1 })
        .reduce(|| 0, |a, b| a + b);
    return (airline.clone(), (count as f64 / size as f64) * 100f64);
}

pub fn ontime_rank(fname: &str) -> Vec<(String, f64)> {
    let mut file = File::open(fname).unwrap();
    let mut data = String::new();
    file.read_to_string(&mut data).ok();
    let lines: Vec<&str> = data.lines().skip(1).collect();
    let parsed: Vec<(String, i64)> = lines
        .par_iter()
        .map(|line| parser(line.to_string()))
        .collect();
    let data = parsed.into_iter().into_group_map();
    let percentages: Vec<(&str, f64)> = data
        .par_iter()
        .map(|airline| check_ontime(airline.0, airline.1))
        .collect();
    return merge_sort(percentages)
        .par_iter()
        .map(|airline| (airline.0.to_string(), airline.1))
        .collect();
}

async fn zip_download(path: PathBuf) -> Result<()> {
    let target = "https://cs.muic.mahidol.ac.th/~ktangwon/2008.csv.zip";
    let res = reqwest::get(target).await?;

    let mut dest = {
        let fname = res
            .url()
            .path_segments()
            .and_then(|segments| segments.last())
            .and_then(|name| if name.is_empty() { None } else { Some(name) })
            .unwrap_or("tmp.bin");
        println!("File to download: '{}'", fname);
        let fname = path.join(fname);
        println!("File location: {:?}", fname);
        File::create(fname)?
    };

    let content = res.bytes().await?;
    copy(&mut content.as_ref(), &mut dest)?;
    Ok(())
}

fn unzipper(path: &mut PathBuf) -> zip::result::ZipResult<()> {
    let mut zip_file_path = File::open(path.join("2008.csv.zip"))?;
    let mut buffer = Vec::new();
    zip_file_path.read_to_end(&mut buffer)?;
    let mut zip_file = zip::ZipArchive::new(zip_file_path)?;
    let mut csv_file = zip_file.by_index(0)?;
    println!("Unzipping File: {:?}", csv_file.name());
    let mut csv_path = File::create(path.join("2008.csv"))?;
    copy(&mut csv_file, &mut csv_path)?;
    Ok(())
}

#[tokio::main]
async fn main() -> Result<()> {
    let mut buffer = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    buffer.push("resources/data/");
    let path = buffer.clone();

    match zip_download(path.clone()).await {
        Ok(_) => println!("Done Download!!"),
        Err(err) => println!("Error Download: {:?}", err),
    };

    match unzipper(&mut path.clone()) {
        Ok(_) => println!("Done Unzip"),
        Err(err) => println!("Error unzip: {:?}", err),
    };

    buffer.push("2008.csv");
    let csv = buffer.clone().into_os_string().into_string().ok();
    let csv_str = csv.as_deref().unwrap_or("");

    let result = ontime_rank(&csv_str);

    for airline in result {
        println!("Airline: {:?}, Percentage: {:?} %", airline.0, airline.1);
    }

    Ok(())
}
