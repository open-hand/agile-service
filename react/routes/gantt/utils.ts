/* eslint-disable no-underscore-dangle */
import { Gantt } from '@choerodon/gantt';
import { produce } from 'immer';
import { findIndex } from 'lodash';
import { IGanttMoveRequestData, IGanttMoveRequestDataPreviousWithNext } from '@/api';
import type { IGanttDimensionTypeValue } from '.';

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
