.. _{{ node |anchorfy }}:

{% block title %}{{ node.node-name | format:"``~(~a~)``" | title:3 }}{% endblock %}
{% block metadata %}{% endblock %}
{{ node.node-docstring |remove-unintentional-whitespace }}
