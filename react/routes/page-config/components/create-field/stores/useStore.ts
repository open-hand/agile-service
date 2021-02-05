import { useLocalStore } from 'mobx-react-lite';
import { axios } from '@choerodon/boot';

export default function useStore(type: string, id: string | number, orgId: string) {
  return useLocalStore(() => ({
    apiGetway: `/agile/v1/${type}s/${id}`,
    iamGetway: `/iam/choerodon/v1/${type}s/${id}`,
    orgId,
    eternalContext: [] as string[],
    checkCode(code: string, schemeCode: string) {
      return axios.get(`${this.apiGetway}/object_scheme_field/check_code?code=${code}&organizationId=${this.orgId}&schemeCode=${schemeCode}`);
    },
    checkName(name: string, schemeCode: string) {
      return axios.get(`${this.apiGetway}/object_scheme_field/check_name?name=${name}&organizationId=${this.orgId}&schemeCode=${schemeCode}`);
    },
    getUsers(param: any, userId: string | number) {
      return axios.get(`${this.iamGetway}/users?param=${param}${userId ? `&id=${userId}` : ''}`);
    },

  }));
}
export type Store = ReturnType<typeof useStore>;
