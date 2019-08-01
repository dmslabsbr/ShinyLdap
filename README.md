## ShinyLdap
Shiny Ldap Login Client

**Author:** dmslabsbr
**Email:** suporte@neoage.com.br

Use the `devtools` package to install the development version of `ShinyLdap`:

##### Installing:
```r
install(devtools)
devtools::install_github('dmslabsbr/ShinyLdap')
```
###### Run Demonstration:
```r
library(shinyldap)
shinyldap::shinyLdap_demo()
```


OBS: You need ldap-utils (command ldapsearch) in your linux server to use this package.

```bash
sudo apt update
sudo apt install ldap-utils
```

##### Example
```r
ShinyLdap::ldap_login(input, output,
      ui_name = 'ui_login',
      modal = TRUE,
      ldap.url = secrets.ldap.url,
      ldap.dc = secrets.ldap.dc,
      ldap.filtro = secrets.ldap.filtro,
      ldap.dominio = secrets.ldap.dominio,
      ldap.campos = secrets.ldap.campos,
      label.user = 'Usuário',
      label.pass = 'Senha',
      label.button.go = 'Login',
      label.button.cancel = 'Cancel',
      label.title = 'Shiny LDAP Login',
      callback.return = ldap.callback.return)
```

#### Licence

> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
