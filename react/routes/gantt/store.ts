/* eslint-disable no-shadow */
/* eslint-disable no-param-reassign */
/* eslint-disable no-underscore-dangle */
import { createRef } from 'react';
import {
  observable, action,
} from 'mobx';
import { GanttRef, Gantt } from '@choerodon/gantt';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import { DetailContainerProps } from '@/components/detail-container';
import openDescriptionConfirm from '@/components/detail-container/openDescriptionConfirm';
import { GanttIssue } from './types';
// 视图日视图、周视图、月视图、季视图、年视图
export const units = [
  {
    type: 'day',
    label: '日',
  },
  {
    type: 'week',
    label: '周',
  },
  {
    type: 'month',
    label: '月',
  },
  {
    type: 'quarter',
    label: '季',
  },
  {
    type: 'halfYear',
    label: '年',
  },
];
class GanttStore {
  ganttRef = createRef<GanttRef>();

  projectId:string|undefined;

  constructor({ projectId }:{projectId?:string}) {
    this.projectId = projectId;
  }

  @observable unit: Gantt.Sight = 'day'

  @observable issueId: string | null = null

  @observable issue: Pick<GanttIssue, 'issueId'|'projectId'> | null = null

  @observable sprintIds: string[] | null = null

  @action
  switchUnit(unit: Gantt.Sight) {
    this.unit = unit;
  }

  @action
  setIssue(issue: Pick<GanttIssue, 'issueId'|'projectId'> | null) {
    const setData = () => {
      this.issueId = issue?.issueId || null;
      this.issue = issue;
    };
    if (!this.detailProps.descriptionChanged) {
      setData();
    } else {
      openDescriptionConfirm({
        onOk: () => {
          setData();
          if (this.detailProps.setDescriptionChanged) {
            this.detailProps.setDescriptionChanged(false);
          }
        },
      });
    }
  }

  @observable programId: string | null = null

  @action
  setProgramId(programId: string | null) {
    this.programId = programId;
  }

  @action
  setSprintIds(sprintIds: string[] | null) {
    this.sprintIds = sprintIds;
    localPageCacheStore.project(this.projectId).setItem('gantt.search.sprints', sprintIds);
  }

  @observable detailProps = {} as DetailContainerProps;

  @action setDetailProps = (data: DetailContainerProps) => {
    this.detailProps = data;
  }
}

export default GanttStore;
