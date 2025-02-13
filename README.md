<img width="1280" alt="Json2Sql" src="/.github/assets/banner.png">

# Json to Sql converter

A simple json parser that converts json to sql queries. All the parsed queries are exported to a `.sql` file. This project is written in Haskell Programming language. All the parsers have been written from scratch and no external library has been used.

## Features

- Supports Table Creation
- Supports Insertion of data
- Handles arrays and nested objects
- Supports multiple data types
- Transcation Sql queries

## Installing

Open terminal and run the following commands:

```bash
curl -fsSL https://raw.githubusercontent.com/2002Bishwajeet/json-to-sql/main/setup.sh | bash
```

## Usage

After installation, you can use the `json-to-sql` command to convert a JSON file to SQL. The basic usage is as follows:

```bash
json-to-sql <file> [table] [--normalize] [dest]
```

- `file`: The JSON file to convert.
- `table`: (Optional) The table name for the SQL file. If not provided, the filename will be used.
- `--normalize`: (Optional) Normalize the JSON data.
- `dest`: (Optional) Destination path for the SQL file. If not provided, the current directory will be used.

### Examples

Convert a JSON file to SQL with default settings:

```bash
json-to-sql data.json
```

Convert a JSON file to SQL with a specified table name:

```bash
json-to-sql data.json my_table
```

Convert a JSON file to SQL with normalization:

```bash
json-to-sql data.json --normalize
```

Convert a JSON file to SQL and save the output to a specific directory:

```bash
json-to-sql data.json my_table --normalize /path/to/output
```

## Motivation

The motivation behind this project was to learn functional programming language and this was the best way for me. I chose to implement this project idea as I needed to get some base knowledge around parsers and improve the current online json2sql parsers we have on the web. I won't recommend this to use for complex jsons but for simple jsons, this should work fine.

## Contributing

Contributions are highly Welcomed 💙 . Feel free to open PRs for small issues such as typos. For large issues or features, please open an issue and wait for it to be assigned to you.

## License

This project is open source and available under the [ BSD-3-Clause](LICENSE).
