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
  ganttRef = createRef<GanttRef>()

  @observable unit: Gantt.Sight = 'day'

  @observable issueId: string | null = null

  @observable sprintIds: string[] | null = null

  @observable createIssueVisible: boolean = false

  @action
  switchUnit(unit: Gantt.Sight) {
    this.unit = unit;
  }

  @action
  setIssueId(issueId: string | null) {
    const setData = () => {
      this.issueId = issueId;
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

  @action
  setSprintIds(sprintIds: string[] | null) {
    this.sprintIds = sprintIds;
    localPageCacheStore.setItem('gantt.search.sprints', sprintIds);
  }

  @action
  setCreateIssueVisible(createIssueVisible: boolean) {
    this.createIssueVisible = createIssueVisible;
  }

  @observable detailProps = {} as DetailContainerProps;

  @action setDetailProps = (data: DetailContainerProps) => {
    this.detailProps = data;
  }
}

export default GanttStore;
