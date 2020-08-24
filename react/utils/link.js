import { stores } from '@choerodon/boot';
import LINK_URL from '@/constants/LINK_URL';
import { getOrganizationId } from './common';
import to from './to';

const { AppState } = stores;

// eslint-disable-next-line no-restricted-globals
export function getParams(url = location.href) {
  const theRequest = {};
  if (url.indexOf('?') !== -1) {
    const str = url.split('?')[1];
    const strs = str.split('&');
    for (let i = 0; i < strs.length; i += 1) {
      theRequest[strs[i].split('=')[0]] = decodeURI(strs[i].split('=')[1]);
    }
  }
  return theRequest;
}

export function commonLink(link) {
  const menu = AppState.currentMenuType;
  const {
    type, id: projectId, name, organizationId,
  } = menu;

  return encodeURI(`/agile${link}?type=${type}&id=${projectId}&organizationId=${organizationId}&orgId=${organizationId}&name=${name}`);
}
export function issueLinkTo(issueId, issueName = null) {
  to(LINK_URL.workListIssue, {
    params: {
      paramIssueId: issueId,
      paramName: issueName,
    },
  });
}
export function toIssueInProject({
  issueId, issueNum, projectId, projectName, category,
}) {
  return encodeURI(`/agile/work-list/issue?type=${'project'}&id=${projectId}&name=${projectName}&category=${category}&organizationId=${getOrganizationId()}&paramIssueId=${issueId}&paramName=${issueNum}`);
}
export function programIssueLink(issueId, issueName, projectId) {
  const menu = AppState.currentMenuType;
  const {
    type, id, name, organizationId, category,
  } = menu;
  return encodeURI(`/program/feature?type=${type}&id=${projectId || id}&name=${name}&category=${category}&organizationId=${organizationId}&paramIssueId=${issueId}&paramName=${issueName}&orgId=${organizationId}`);
}
export function testExecuteLink(executeId) {
  const menu = AppState.currentMenuType;
  const {
    type, id, name, organizationId, category,
  } = menu;
  return encodeURI(`/testManager/TestExecute/execute/${executeId}?type=${type}&id=${id}&name=${name}&category=${category}&organizationId=${organizationId}&orgId=${organizationId}`);
}
