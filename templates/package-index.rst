{{ package.name |format:"~(~a~) package" |title }}

{{ package.docstring }}

{% if variables %}
---------
Variables
---------
{% for variable in variables %}{% include "variable.rst" %}{% endfor %}{% endif %}

{% if functions %}
---------
Functions
---------
{% for function in functions %}
{% include "function.rst" %}
{% endfor %}{% endif %}

{% if macros %}
------
Macros
------
{% for macro in macros %}{% include "macro.rst" %}{% endfor %}{% endif %}


{% if generic-functions %}
-----------------
Generic Functions
-----------------
{% for generic-function in generic-functions %}{% include "generic-function.rst" %}{% endfor %}{% endif %}


{% if structures %}
----------
Structures
----------
{% for structure in structures %}{% include "generic-function.rst" %}{% endfor %}{% endif %}

{% if classes %}
-------
Classes
-------
{% for class in classes %}{% include "class.rst" %}{% endfor %}{% endif %}
