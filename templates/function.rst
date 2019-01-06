{% extends "base.rst" %}

{% block metadata %}{% if node.lambda-list %}``{{ node.lambda-list }}``{% else %}{% endif %}
{% endblock %}
