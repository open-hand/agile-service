import to from '@/utils/to';

interface IRouterToList {
  issueLinkTo: (issueId: string, issueName: string | null, otherParams?: any) => void,
}

function issueLinkTo(issueId: string, issueName: string | null = null, otherParams: any = {}) {
  to(LINK_URL.workListIssue, {
    type: 'project',
    params: {
      paramIssueId: issueId,
      paramName: issueName,
      ...otherParams,
    },
  });
}

const LINK_URL = {
  knowledgeDoc: (baseId: string) => `/knowledge/project/doc/${baseId}`,
  workListIssue: '/agile/work-list/issue',
  workListBacklog: '/agile/work-list/backlog',
  workListVersion: '/agile/project-version',
  versionRelease: '/agile/project-version/release',
  versionPublish: '/agile/project-version/publish',
  report: '/charts',
  reportBurnDown: '/agile/reporthost/burndownchart',
  reportIssueType: '/agile/reporthost/pieReport/typeCode',
  reportPriority: '/agile/reporthost/pieReport/priority',
  reportAssignee: '/agile/reporthost/pieReport/assignee',
  reportSprint: '/agile/reporthost/sprintReport',
  scrumboard: '/agile/scrumboard',
  scrumboardSetting: '/agile/scrumboard/setting',
  iterationBoard: (sprintId: string) => `/agile/iterationBoard/${sprintId}`,
  stateMachine: '/agile/state-machine',
  pageConfig: '/agile/page/config',
  status: '/agile/state-machine',
};

const LINK_URL_TO: IRouterToList = {
  issueLinkTo,
};

export function getParams(url: string) {
  const theRequest: { [propsName: string]: any } = {};
  if (url.indexOf('?') !== -1) {
    const str = url.split('?')[1];
    const strs = str.split('&');
    for (let i = 0; i < strs.length; i += 1) {
      theRequest[strs[i].split('=')[0]] = decodeURI(strs[i].split('=')[1]);
    }
  }
  return theRequest;
}
export { LINK_URL_TO };
export default LINK_URL;
