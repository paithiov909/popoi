# siliconate

<!-- badges: start -->
<!-- badges: end -->

> Wrapper Function of Aloxaf/silicon

## System Requirements

- [Aloxaf/silicon: Create beautiful image of your source code.](https://github.com/Aloxaf/silicon)

## Usage

```r
reprex::reprex({
  ## > Run a bit of R code using rmarkdown::render() and write the rendered result to user's clipboard.
  x <- 1:4
  y <- 2:5
  x + y
}, venue = "R", html_preview = FALSE)

siliconate::silicon() ## Call silicon command. By default, the image just goes to clipboard.
```

Then you can paste and save the image like this.

![exsample](https://raw.githack.com/paithiov909/siliconate/main/man/figures/siliconate_exsample.jpg)

## License

MIT license. 

