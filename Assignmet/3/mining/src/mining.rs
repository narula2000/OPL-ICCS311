extern crate reqwest;
use error_chain::error_chain;
use std::fs::File;
use std::io::copy;
use std::io::Read;
use std::path::PathBuf;

error_chain! {
    foreign_links {
        Io(std::io::Error);
        HttpRequest(reqwest::Error);
    }
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

    Ok(())
}
