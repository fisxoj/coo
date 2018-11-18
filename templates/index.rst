{{ system.name |title }}

{% if system.author %}:author: {{ system.author }}{% endif %}
{% if system.version %}:version: {{ system.version }}{% endif %}
{% if system.licence %}:license: {{ system.licence }}{% endif %}
{% if system.homepage %}:homepage: {{ system.homepage }}{% endif %}

{% if system.long-description %}
{{ system.long-description }}
{% else %}
{{ system.description }}
{% endif %}

--------
Packages
--------
{% for package in index.packages %}
* `{{ package.name |lower }} <{{ package |linkify }}>`_
{% endfor %}
