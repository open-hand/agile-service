import { getProjectId, getApplyType } from './common';

export function disableIssueEdit(projectId: string): boolean {
  const same = String(projectId) === String(getProjectId());
  const isInProgram = getApplyType() === 'program';
  // 项目群可编辑子项目issue
  if (isInProgram) {
    return false;
  }
  // 子项目之间不可互相编辑，也不可编辑项目群issue
  return !same;
}
export function disableDemandEdit(projectId: string): boolean {
  const same = String(projectId) === String(getProjectId());
  const isInProgram = getApplyType() === 'program';
  // 项目群可编辑子项目issue
  if (isInProgram) {
    return false;
  }
  // 子项目之间不可互相编辑，也不可编辑项目群issue
  return !same;
}
