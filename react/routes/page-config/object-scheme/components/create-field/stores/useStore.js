import { useLocalStore } from 'mobx-react-lite';
import { axios } from '@choerodon/boot';

export default function useStore(type, id, orgId) {
  return useLocalStore(() => ({
    apiGetway: `/agile/v1/${type}s/${id}`,
    iamGetway: `/base/v1/${type}s/${id}`,
    orgId,
    
    checkCode(code, schemeCode) {
      return axios.get(`${this.apiGetway}/object_scheme_field/check_code?code=${code}&organizationId=${this.orgId}&schemeCode=${schemeCode}`);
    },
    checkName(name, schemeCode) {
      return axios.get(`${this.apiGetway}/object_scheme_field/check_name?name=${name}&organizationId=${this.orgId}&schemeCode=${schemeCode}`);
    },
    getUsers(param, userId) { 
      return axios.get(`${this.iamGetway}/users?param=${param}${userId ? `&id=${userId}` : ''}`); 
    },

  }));
}
