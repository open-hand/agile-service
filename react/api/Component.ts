import { stores, axios } from '@choerodon/boot';
import { getProjectId } from '../common/utils';

const { AppState } = stores;

class ComponentApi {
  get prefix() {
    return `/agile/v1/projects/${getProjectId()}`;
  }
  loadComponents(pagination: any, filters: object, componentId: number) {
    const { current, pageSize } = pagination;
    const page = current;
    const size = pageSize;
    if (componentId) {
      // return axios.post(`${this.prefix}/component/query_all?componentId=${componentId}&page=${page}&size=${size}&no_issue_test=true`, filters);
      return axios.post(`${this.prefix}/component/query_all`, filters, {
       params: {
        componentId,
        page,
        size,
        no_issue_test: true,
       }
      });

    }
    return axios.post(`${this.prefix}/component/query_all?no_issue_test=true&page=${page}&size=${size}`, filters);
  }

  loadAllComponents() {
    return axios.post(
      `${this.prefix}/component/query_all?size=${999}&page=${1}`, {
        advancedSearchArgs: {},
        searchArgs: {},
        content: '',
      },
    );
  }
  
  createComponent(obj: any) {
    const projectId = AppState.currentMenuType.id;
    const component = {
      projectId,
      ...obj,
    };
    return axios.post(
      `${this.prefix}/component`,
      component,
    );
  }
  
  updateComponent(componentId: number, obj: object) {
    const projectId = AppState.currentMenuType.id;
    const component = {
      projectId,
      ...obj,
    };
    return axios.put(
      `${this.prefix}/component/${componentId}`,
      component,
    );
  }
  
  loadComponent(componentId: number) {
    return axios.get(`${this.prefix}/component/${componentId}`);
  }
  
  deleteComponent(componentId: number, relateComponentId: number) {
    if (relateComponentId === 0) {
      return axios.delete(`${this.prefix}/component/${componentId}`);
    }
    return axios.delete(`${this.prefix}/component/${componentId}?relateComponentId=${relateComponentId}`);
  }  
}

const componentApi = new ComponentApi();

export { componentApi };