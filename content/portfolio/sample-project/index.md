---
date: "2019-05-02T19:47:09+02:00"
description: This is the description of our sample project
jobDate: 2021
techs:
- Rstats
testimonial:
  image: sample-project/pp.jpg
  name: Cleiton Rocha
  role: Data Analyst
  text: Graduating in Economics at UFBA | Data analyst at the Superintendency of Economic and Social Studies of Bahia - SEI | CIDACS collaborator in projects related to COVID-19 | Focus on Economic Development and Finance.
thumbnail: sample-project/decretos_covid.png
title: Utilizando o R para obter decretos relacionados ao COVID-19 no Brasil
work:
- Rstats
- COVID-19
---


### Introdução


No Brasil, diante da emergência sanitária mundial, as autoridades estabeleceram diversas portarias/decretos com regras e normas para funcionamento de serviços de saúde e serviços não essenciais.

Na maioria das unidades da federação, as aulas na rede pública e na rede privada foram suspensas. Eventos com grande número de pessoas foram proibidos. Houve mudanças no transporte público, com redução de frota, e alterações nas regras de abertura de comércios, bares, restaurantes e shoppings.

O objetivo dessas restrições era de evitar a sobrecarga dos serviços de saúde e esgotamento dos leitos de tratamento, à medida que, a União, Estados e Municípios poderiam se preparar para o aumento no número de casos com a construção de hospitais de campanha e importação de respiradores.
Ao longo dos últimos meses os decretos vêm sendo moldados de acordo com a realidade da pandemia no país. Mudanças como aumento das restrições de circulação e expansão na oferta de crédito, foram mais recentemente transformadas em medidas de reabertura de algumas atividades econômicas com estruturas e horários de funcionamento diferenciados, entre outros tipos. Uma forma de olhar o panorama do Covid-19 no país é observando como os agentes políticos legislaram a respeito do tema.
No texto a seguir explico como gerar um banco de dados no R com informações atualizadas sobre os decretos nos níveis municipal, estadual e federal, com registro da data, sua classificação em decreto ou lei e algumas outras informações.
Vamos lá!

