import { stores } from '@choerodon/boot';
import { getOrganizationId } from './common';

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
export function issueLink(issueId, typeCode, issueName = null) {
  const menu = AppState.currentMenuType;
  const {
    type, id: projectId, name, organizationId, category,
  } = menu;
  if (typeCode === 'issue_test' || typeCode === 'issue_auto_test') {
    return encodeURI(`/testManager/IssueManage/testCase/${issueId}?type=${type}&id=${projectId}&name=${name}&category=${category}&organizationId=${organizationId}&orgId=${organizationId}`);
  } else if (issueName) {
    return encodeURI(`/agile/work-list/issue?type=${type}&id=${projectId}&name=${name}&category=${category}&organizationId=${organizationId}&paramIssueId=${encodeURIComponent(issueId)}&paramName=${issueName}&orgId=${organizationId}`);
  } else {
    return encodeURI(`/agile/work-list/issue?type=${type}&id=${projectId}&name=${name}&category=${category}&organizationId=${organizationId}&paramIssueId=${encodeURIComponent(issueId)}&orgId=${organizationId}`);
  }
}
export function toIssueInProject({
  issueId, issueNum, projectId, projectName, category,
}) {
  return encodeURI(`/agile/work-list/issue?type=${'project'}&id=${projectId}&name=${projectName}&category=${category}&organizationId=${getOrganizationId()}&paramIssueId=${encodeURIComponent(issueId)}&paramName=${issueNum}`);
}
export function programIssueLink(issueId, issueName, projectId) {
  const menu = AppState.currentMenuType;
  const {
    type, id, name, organizationId, category,
  } = menu;
  return encodeURI(`/program/feature?type=${type}&id=${projectId || id}&name=${name}&category=${category}&organizationId=${organizationId}&paramIssueId=${encodeURIComponent(issueId)}&paramName=${issueName}&orgId=${organizationId}`);
}
export function testExecuteLink(executeId) {
  const menu = AppState.currentMenuType;
  const {
    type, id, name, organizationId, category,
  } = menu;
  return encodeURI(`/testManager/TestExecute/execute/${executeId}?type=${type}&id=${id}&name=${name}&category=${category}&organizationId=${organizationId}&orgId=${organizationId}`);
}
