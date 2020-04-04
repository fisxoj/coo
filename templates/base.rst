.. _{{ node |anchorfy }}:

{% block title %}{{ node.node-name | format:"``~(~a~)``" | title:3 }}{% endblock %}
{{ node |metadata }}
{{ node.node-docstring |remove-unintentional-whitespace }}
