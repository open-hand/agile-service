/* eslint-disable no-underscore-dangle */
import { Gantt } from '@choerodon/gantt';
import { produce } from 'immer';
import { findIndex, noop } from 'lodash';
import { IGanttMoveRequestData, IGanttMoveRequestDataPreviousWithNext } from '@/api';
import type { IGanttDimensionTypeValue } from '.';
import { Issue, User } from '@/common/types';
import { list2tree } from '@/utils/tree';

/**
 * 获取甘特图移动后待提交到后端数据
 *
 * @param param0
 * @returns  返回 undefined 则代表此次移动无效
 */
export function getGanttMoveSubmitData({
  sourceBar, type, destinationBar, flattenData,
}: { sourceBar: Gantt.Bar, destinationBar: Gantt.Bar, type: IGanttDimensionTypeValue, flattenData: Gantt.Bar[] }): (IGanttMoveRequestData | undefined) {
  function getInstanceObject() {
    const instanceId = sourceBar._depth > 0 ? sourceBar._parent?.record.issueId : 0;
    switch (type) {
      case 'task':
        return {
          instanceType: 'task',
          instanceId,
        };
      case 'assignee': {
        if (sourceBar._depth === 0) {
          return {};
        }
        return sourceBar._depth === 1 ? {
          instanceType: 'assignee',
          instanceId: sourceBar._parent?.record.assigneeId,
        } : {
          instanceType: 'task',
          instanceId,
        };
      }
      case 'sprint': {
        if (sourceBar._depth === 0) {
          return {};
        }
        return sourceBar._depth === 1 ? {
          instanceType: 'sprint',
          instanceId: sourceBar._parent?.record.sprintId,
        } : {
          instanceType: 'task',
          instanceId,
        };
      }
      case 'epic': {
        if (sourceBar._depth === 1) {
          return {
            instanceType: 'epic',
            instanceId: sourceBar._parent?.record.issueId,
          };
        }
        return sourceBar._parent?._depth === 1 && sourceBar._parent?.record.groupType === 'feature' ? {
          instanceType: 'feature',
          instanceId: sourceBar._parent?.record.issueId,
        } : {
          instanceType: 'task',
          instanceId,
        };
      }
      default:
        break;
    }
    return {
      instanceType: 'task',
      instanceId: '0',
    };
  }

  //  是否有上层级
  const instanceObject = getInstanceObject();
  function getSameDepthBar(depth: number, bar?: Gantt.Bar): Gantt.Bar | undefined {
    console.log('getSameDepthBar', bar?._depth, bar?._depth === depth);
    return bar?._depth === depth ? bar : undefined;
  }
  function getRecordId(r?: Gantt.Record) {
    if (['sprint', 'assignee'].includes(type) && sourceBar._depth === 0) {
      return r?.sprintId || r?.assigneeId;
    }
    return r?.issueId;
  }
  // 上一个 下一个
  const previousAndNextIdObject = {} as IGanttMoveRequestDataPreviousWithNext;
  if (sourceBar.absoluteIndex > destinationBar.absoluteIndex) {
    const previousRecord = getSameDepthBar(sourceBar._depth, flattenData[destinationBar.absoluteIndex - 1])?.record;
    previousAndNextIdObject.previousId = getRecordId(previousRecord);
    previousAndNextIdObject.nextId = getRecordId(destinationBar.record);
  } else {
    previousAndNextIdObject.previousId = getRecordId(destinationBar.record);
    previousAndNextIdObject.nextId = getRecordId(getSameDepthBar(sourceBar._depth, flattenData[destinationBar.absoluteIndex + 1])?.record);
  }

  const requestData: IGanttMoveRequestData = {
    dimension: type,
    currentId: sourceBar.record.issueId,
    ...instanceObject,
    ...previousAndNextIdObject,
  };
  //  未分配永远是最后一个
  requestData.nextId = Number(requestData.nextId) === 0 ? undefined : requestData.nextId;
  if (Number(requestData.previousId) === 0) {
    return undefined;
  }
  if (['sprint', 'assignee'].includes(type) && sourceBar._depth === 0) {
    requestData.currentId = sourceBar.record.sprintId || sourceBar.record.assigneeId;
  }
  return requestData;
}

function moveData(data: any[], sourceIndex: number, destinationIndex: number) {
  if ((sourceIndex + destinationIndex) < 0) {
    return false;
  }
  //   const startIndex = destinationIndex < 0 ? data.length + destinationIndex : destinationIndex;
  const delItem = data.splice(sourceIndex, 1)[0];
  data.splice(destinationIndex, 0, delItem);
  return true;
}
export function getGanttMoveDataOrigin({ type, sourceBar }: { sourceBar: Gantt.Bar, type: IGanttDimensionTypeValue }): 'rankList' | 'data' {
  return ['sprint', 'assignee'].includes(type) && sourceBar._depth === 0 ? 'rankList' : 'data';
}
export function ganttLocalMove({
  sourceBar, data, destinationBar, type,
}: { data: any[], sourceBar: Gantt.Bar, destinationBar: Gantt.Bar, type: IGanttDimensionTypeValue }) {
  let success = false;
  // 这里对 data 原数据进行移动，避免创建删除等操作导致排序混乱
  const newData = produce(data, (draft) => {
    let sourceDataIndex = -1;
    let destinationDataIndx = -1;
    if (type === 'sprint' && sourceBar._depth === 0) {
      // 冲刺移动
      sourceDataIndex = findIndex(draft, (a) => a === sourceBar.record.sprintId);
      destinationDataIndx = findIndex(draft, (a) => a === destinationBar.record.sprintId);
    } else if (type === 'assignee' && sourceBar._depth === 0) {
      // 经办人移动
      sourceDataIndex = findIndex(draft, (a) => a === sourceBar.record.assigneeId);
      destinationDataIndx = findIndex(draft, (a) => a === destinationBar.record.assigneeId);
    } else {
      // 问题移动
      sourceDataIndex = findIndex(draft, { issueId: sourceBar.record.issueId });
      destinationDataIndx = findIndex(draft, { issueId: destinationBar.record.issueId });
    }
    success = moveData(draft, sourceDataIndex, destinationDataIndx);
  });
  return { success, newData };
}

const ganttList2Tree = (data: any[]) => list2tree(data, { valueField: 'issueId', parentField: 'parentId' });

const formatData = (data: any[]) => data.map((item, i, arr) => {
  let newItem = Object.assign(item, {});
  if (item.parentId && item.parentId !== '0' && !arr.find((issue) => issue.issueId === item.parentId)) {
    Object.assign(newItem, { parentId: '0' });
  }
  if (item.epicId && item.epicId !== '0' && !arr.find((issue) => issue.issueId === item.epicId)) {
    Object.assign(newItem, { epicId: '0' });
  }
  if (item.featureId && item.featureId !== '0' && !arr.find((issue) => issue.issueId === item.featureId)) {
    Object.assign(newItem, { featureId: null });
  }
  if ((item.create || item.issueTypeVO.typeCode === 'sub_task' || item.issueTypeVO.typeCode === 'bug') && item.parentId) {
    const parent = arr.find((issue) => issue.issueId === item.parentId);
    const newParent = Object.assign(parent, {});
    if (parent.epicId && parent.epicId !== '0' && !arr.find((issue) => issue.issueId === parent.epicId)) {
      Object.assign(newParent, { epicId: '0' });
    }
    if (parent.featureId && parent.featureId !== '0' && !arr.find((issue) => issue.issueId === parent.featureId)) {
      Object.assign(newParent, { featureId: null });
    }
    newItem = {
      ...newItem,
      ...{
        epicId: newParent?.epicId,
        featureId: newParent?.featureId,
      },
    };
  }
  return newItem;
});

export const ganttNormalizeIssue = (issue: Issue, source: any = {}) => Object.assign(source, {
  parentId: issue.relateIssueId || issue.parentIssueId,
  estimatedEndTime: issue.estimatedEndTime,
  estimatedStartTime: issue.estimatedStartTime,
  issueTypeVO: issue.issueTypeVO,
  objectVersionNumber: issue.objectVersionNumber,
  statusVO: issue.statusVO,
  summary: issue.summary,
  actualCompletedDate: issue.actualCompletedDate,
  completed: issue.completed,
  issueId: issue.issueId,
  assignee: issue.assigneeId ? {
    name: issue.assigneeName,
    realName: issue.assigneeRealName,
  } : null,
  sprint: issue.activeSprint,
  featureId: issue.featureId,
  epicId: issue.epicId,
});

const groupByTask = (data: any[]) => ganttList2Tree(data);
const groupByUser = (data: any[], rankList: string[]) => {
  const collectIdSet = new Set<string>();
  const map = new Map<string, { user?: User, rank: number, children: any[] }>(rankList.map((item, index) => [item, { rank: index, children: [] }]));
  const noAssigneeData: any[] = [];
  data.forEach((issue) => {
    if (issue.assignee?.id) {
      if (collectIdSet.has(issue.assignee.id)) {
        map.get(issue.assignee.id)?.children.push(issue);
      } else {
        collectIdSet.add(issue.assignee.id);
        map.set(issue.assignee.id, { ...map.get(issue.assignee.id)!, user: issue.assignee, children: [issue] });
      }
    } else {
      noAssigneeData.push(issue);
    }
  });
  if (noAssigneeData.length > 0) {
    map.set('0', { user: { id: '0', name: '未分配' } as User, rank: rankList.length, children: noAssigneeData });
  }
  return [...map.entries()].map(([assigneeId, { user, children }]) => ({
    summary: user?.name,
    group: true,
    assigneeId: String(assigneeId),
    disabledDrag: assigneeId === '0',
    groupType: 'assignee',
    children: ganttList2Tree(children),
  }));
};
const groupBySprint = (data: any[], rankList: string[]) => {
  const collectIdSet = new Set<string>();
  const map = new Map<string, { sprint?: any, rank?: number, children: any[], disabledDrag?: boolean }>(rankList.map((item, index) => [item, { rank: index, children: [] }]));
  const noSprintData: any[] = [];
  data.forEach((issue) => {
    if (issue.sprint) {
      if (collectIdSet.has(issue.sprint.sprintId)) {
        map.get(issue.sprint.sprintId)?.children.push(issue);
      } else {
        collectIdSet.add(issue.sprint.sprintId);
        map.set(issue.sprint.sprintId, { ...map.get(issue.sprint.sprintId), sprint: issue.sprint, children: [issue] });
      }
    } else {
      noSprintData.push(issue);
    }
  });
  if (noSprintData.length > 0) {
    map.set('0', { sprint: { sprintId: '0', sprintName: '未分配' }, disabledDrag: true, children: noSprintData });
  }
  return [...map.entries()].map(([sprintId, { sprint, disabledDrag, children }]) => ({
    summary: sprint?.sprintName,
    group: true,
    disabledDrag: !!disabledDrag,
    sprintId,
    groupType: 'sprint',
    groupWidthSelf: true,
    estimatedStartTime: sprint.startDate,
    estimatedEndTime: sprint.endDate,
    children: ganttList2Tree(children),
  }));
};

const groupByFeature = (epicChildrenData: any, data: any) => {
  const map = new Map<string, { feature: any, disabledDrag?: boolean, children: any[] }>();
  const noFeatureData: any[] = [];
  epicChildrenData.forEach((issue: any) => {
    if (issue.featureId && issue.featureId !== '0') {
      const feature = data.find((item: any) => !item.create && item.issueId.toString() === issue.featureId.toString());
      if (map.has(feature?.featureName)) {
        map.get(feature?.featureName)?.children.push(issue);
      } else {
        map.set(feature?.featureName, {
          feature,
          children: [issue],
        });
      }
    } else {
      noFeatureData.push(issue);
    }
  });
  if (noFeatureData.length > 0) {
    map.set('未分配特性', { feature: { issueId: '0' }, disabledDrag: true, children: noFeatureData });
  }

  return [...map.entries()].map(([name, { feature, disabledDrag, children }]) => ({
    group: name === '未分配特性',
    disabledDrag: !!disabledDrag,
    groupType: 'feature',
    summary: name,
    ...feature,
    children: ganttList2Tree(children),
  }));
};

const groupByEpic = (data: any, isInProgram: boolean) => {
  const map = new Map<string, { epic: any, disabledDrag?: boolean, children: any[] }>();
  const noEpicData: any[] = [];
  data.filter((item: any) => item.issueTypeVO?.typeCode !== 'issue_epic' && item.issueTypeVO?.typeCode !== 'feature').forEach((issue: any) => {
    if (issue.epicId && issue.epicId !== '0') {
      const epic = data.find((item: any) => item.issueId === issue.epicId);
      if (map.has(epic?.epicName)) {
        map.get(epic?.epicName)?.children.push(issue);
      } else {
        map.set(epic?.epicName, {
          epic,
          children: [issue],
        });
      }
    } else {
      noEpicData.push(issue);
    }
  });
  if (noEpicData.length > 0) {
    map.set('未分配史诗', { epic: { issueId: '0' }, disabledDrag: true, children: noEpicData });
  }

  return [...map.entries()].map(([name, { epic, children, disabledDrag }]) => ({
    group: name === '未分配史诗',
    disabledDrag: !!disabledDrag,
    groupType: 'epic',
    summary: name,
    ...epic,
    children: isInProgram ? groupByFeature(children, data) : ganttList2Tree(children),
  }));
};

export const ganttDataGroupByType = ({
  data, type, rankList, isInProgram,
}: { data: any[], rankList?: string[], type: IGanttDimensionTypeValue, isInProgram: boolean }) => {
  switch (type) {
    case 'assignee': {
      if (!rankList) {
        return [];
      }
      return groupByUser(data, rankList);
    }
    case 'epic': {
      const formattedData = formatData(data);
      return groupByEpic(formattedData, isInProgram);
    }
    case 'task':
      return groupByTask(data);
    case 'sprint': {
      if (!rankList) {
        return [];
      }
      return groupBySprint(data, rankList);
    }
    default:
      break;
  }
  return data;
};
