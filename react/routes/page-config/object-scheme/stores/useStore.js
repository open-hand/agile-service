import { useLocalStore } from 'mobx-react-lite';
import { axios } from '@choerodon/boot';

export default function useStore(type, id, orgId) {
  return useLocalStore(() => ({
    apiGetway: `/agile/v1/${type}s/${id}`,
    iamGetway: `/iam/choerodon/v1/${type}s/${id}`,
    orgId,
    
    updateField(fieldId, field) {
      return axios.put(`${this.apiGetway}/object_scheme_field/${fieldId}?organizationId=${this.orgId}`, field);
    },
  }));
}
