import queryString from 'querystring';
import { find } from 'lodash';
import { stores } from '@choerodon/boot';
import { History } from 'history';
import { error } from './log';

const { HeaderStore, AppState } = stores;
let history: History | null;

export function setHistory(newHistory: History) {
  history = newHistory;
}
interface IProject {
  id: number
  name: string
  category: 'GENERAL' | 'PROGRAM'
  organizationId: number
}

interface IOrg {
  id: number
  name: string
}
type Path = string;
type ProjectLocationDescriptor = {
  type: 'project'
  id?: number
  name?: string
  category?: 'GENERAL' | 'PROGRAM'
  organizationId?: number
  params?: {
    [param: string]: string
  }
}
type OrgLocationDescriptor = {
  type: 'org'
  id?: number
  name?: string
  organizationId?: number
  params?: {
    [param: string]: string
  }
}
type SiteLocationDescriptor = {
  type: 'site'
  organizationId?: number
  params?: {
    [param: string]: string
  }
}
type LocationDescriptor =
  ProjectLocationDescriptor |
  OrgLocationDescriptor |
  SiteLocationDescriptor;
const defaultDescriptor: ProjectLocationDescriptor = {
  type: 'project',
};
type IParams = NodeJS.Dict<string | number | boolean | ReadonlyArray<string> |
  ReadonlyArray<number> | ReadonlyArray<boolean> | null>
<<<<<<< HEAD
// eslint-disable-next-line max-len
function getParams(path: Path, descriptor: LocationDescriptor = defaultDescriptor): IParams | null {
  const { type, params: otherParams } = descriptor;
=======
function getParams(path: Path, descriptor: LocationDescriptor = defaultDescriptor): IParams {
  const { type = 'project', params: otherParams } = descriptor;
>>>>>>> [IMP]前端链接整合
  let params;
  switch (type) {
    case 'project': {
      const { id } = descriptor as ProjectLocationDescriptor;
      if (!id) {
        const {
          id: projectId, name, category, organizationId,
        } = AppState.currentMenuType;
        params = {
          type: 'project',
          id: String(projectId),
          name,
          category,
          organizationId: String(organizationId),
        };
      } else {
        const projects: IProject[] = HeaderStore.getProData;
        const targetProject = find(projects, (v) => String(v.id) === String(id));
        if (!targetProject) {
          error('链接错误，未找到目标项目，请检查参数', path, descriptor);
          return null;
        }
        const {
          name,
          category,
          organizationId,
        } = targetProject;
        params = {
          type: 'project',
          id: String(id),
          name,
          category,
          organizationId: String(organizationId),
        };
      }
      break;
    }
    case 'org': {
      const { id } = descriptor as OrgLocationDescriptor;
      if (!id) {
        const {
          id: orgId, name,
        } = AppState.currentMenuType;
        params = {
          type: 'organization',
          id: String(orgId),
          name,
          organizationId: String(orgId),
        };
      } else {
        const orgs: IOrg[] = HeaderStore.getOrgData;
        const targetOrg = find(orgs, (v) => String(v.id) === String(id));
        if (!targetOrg) {
          error('链接错误，未找到目标组织，请检查参数', path, descriptor);
          return null;
        }
        const {
          name,
        } = targetOrg;
        params = {
          type: 'organization',
          id: String(id),
          name,
          organizationId: String(id),
        };
      }
      break;
    }
    case 'site': {
      const {
        organizationId =
        AppState.currentMenuType.organizationId,
      } = descriptor as SiteLocationDescriptor;
      const orgs: IOrg[] = HeaderStore.getOrgData;
      const targetOrg = find(orgs, (v) => String(v.id) === String(organizationId));
      if (!targetOrg) {
        error('链接错误，未找到目标组织，请检查参数', path, descriptor);
        return null;
      }
      params = {
        type: 'site',
        organizationId: String(organizationId),
      };
      break;
    }
    default: {
      error('链接错误，请检查参数', path, descriptor);
      return null;
    }
  }
  const totalParams = {
    ...params,
    ...otherParams,
  };

  return totalParams;
}

const to = (path: Path, descriptor: LocationDescriptor = defaultDescriptor) => {
  const params = getParams(path, descriptor);
  if (!params) {
    return;
  }
  const search = queryString.stringify(params);
  if (!history) {
    error('跳转失败，未设置history');
    return;
  }
  history.push({
    pathname: path,
    search,
  });
};
const linkUrl = (path: Path, descriptor: LocationDescriptor = defaultDescriptor) => {
  const params = getParams(path, descriptor);
  if (!params) {
    return path;
  }
  const search = queryString.stringify(params);
  return `${path}?${search}`;
};
export { linkUrl };
export default to;
