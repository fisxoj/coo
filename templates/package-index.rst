{{ package.name |format:"``~(~a~)`` package" |title }}

`<< back to {{ system.name }} index <{{ root-dir }}index.html>`_

{{ package.docstring }}

Exported symbols:

{% if variables %}
---------
Variables
---------
{% for node in variables %}
{% include "variable.rst" %}
{% endfor %}{% endif %}

{% if functions %}
---------
Functions
---------
{% for node in functions %}
{% include "function.rst" %}
{% endfor %}{% endif %}

{% if macros %}
------
Macros
------
{% for node in macros %}
{% include "macro.rst" %}
{% endfor %}{% endif %}


{% if generic-functions %}
-----------------
Generic Functions
-----------------
{% for node in generic-functions %}
{% include "generic-function.rst" %}
{% endfor %}{% endif %}


{% if structures %}
----------
Structures
----------
{% for node in structures %}
{% include "generic-function.rst" %}
{% endfor %}{% endif %}

{% if classes %}
-------
Classes
-------
{% for node in classes %}
{% include "class.rst" %}
{% endfor %}{% endif %}
