# Startup Scraper App    ![Alt text](image/Lead_funnel.jpg.)

## This is a temporary working description of the app built

### Description

The meta idea of this app is to build a framework for generating leads for independent consultancy business and to provide some analytics to facilitate the leads qualification.  The app is built on the basis of rvest function that scrapes data. The analytical side of the app is based on text mining.
There are two outputs from the text mining:
* Companies categorisation based on topic modelling wit the use of Latent Dirichlet allocation algorithm
* Search engine or just keywords generator for pitch drafting based on simple tf-idf ranking function for the key terms in the companies description

### Restrictions

The main restriction of the app is that it is build for one particular URL. However, as it was mentioned the app is a how to framework. Its logic can be applied to different web sources. Also when applied to different sources the , the data that will be scraped will be different

### Future development

An exciting opportunity for future development of the app is building of a  pitch generator as a natural language generator build with [PyTorch](https://pytorch.org/), [Hugging Face Transformers](https://github.com/huggingface/transformers) or [OpenAI's GPT-2](https://openai.com/)
