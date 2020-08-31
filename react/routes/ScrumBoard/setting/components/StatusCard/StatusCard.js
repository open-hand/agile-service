import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Draggable } from 'react-beautiful-dnd';
import {
  Radio, Icon, Tooltip, Modal,
} from 'choerodon-ui';
import { Button } from 'choerodon-ui/pro';
import { find } from 'lodash';
import { stores, Permission, Choerodon } from '@choerodon/boot';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import STATUS from '@/constants/STATUS';
import './StatusCard.less';
import { boardApi, statusApi } from '@/api';

const { AppState } = stores;
const { confirm, warning } = Modal;
const prefix = 'c7n-scrumsetting-card';
@observer
class StatusCard extends Component {
  getStatusNumber() {
    const data = ScrumBoardStore.getBoardData;
    // 计算所有列下面的状态总数
    const totalStatus = data.reduce((total, column) => total + column.subStatusDTOS.length, 0);
    return totalStatus;
  }

  handleDeleteClick = async () => {
    const { data } = this.props;
    const deleteCode = data.statusId;
    const canBeDeleted = await statusApi.checkCanDelStatus(deleteCode);
    const that = this;
    if (canBeDeleted) {
      confirm({
        title: '移除状态',
        content: `确定要移除状态 ${data.name}？`,
        onOk() {
          that.handleDeleteStatus();
        },
      });
    } else {
      warning({
        title: '移除状态',
        content: `状态 ${data.name}已在其他看板的列中，不可删除。`,
      });
    }
  };

  async handleDeleteStatus() {
    const { data: propData, refresh } = this.props;
    const originData = JSON.parse(JSON.stringify(ScrumBoardStore.getBoardData));
    const data = JSON.parse(JSON.stringify(ScrumBoardStore.getBoardData));
    const deleteCode = propData.statusId;
    let deleteIndex = '';
    for (let index = 0, len = data[data.length - 1].subStatusDTOS.length; index < len; index += 1) {
      if (String(data[data.length - 1].subStatusDTOS[index].statusId) === String(deleteCode)) {
        deleteIndex = index;
      }
    }
    data[data.length - 1].subStatusDTOS.splice(deleteIndex, 1);
    ScrumBoardStore.setBoardData(data);
    try {
      await boardApi.deleteStatus(deleteCode);
    } catch (err) {
      ScrumBoardStore.setBoardData(originData);
    }
    refresh();
  }

  /**
   * 搜索状态code
   * @param {*} statusId
   */
  findStatusCodeByStatusId(statusId) {
    const statusList = ScrumBoardStore.getStatusList;
    return find(statusList, { id: statusId }).code;
  }

  getDisabled = () => {
    const { columnId, data } = this.props;
    // 待处理状态 不可删除 一直处于禁止删除状态
    // 根据状态code 是否为create 为create则如下
    if (this.findStatusCodeByStatusId(data.statusId) === 'create') {
      return [true, '初始化状态'];
    }
    if (columnId === '0') {
      if (data.issues.length === 0) {
        if (this.getStatusNumber() <= 1) {
          return [true, '应至少剩余一个状态'];
        }
        return [false];
      }
      return [true, '状态下有问题'];
    }
    return [true, '在普通列中'];
  }

  handleSetComplete = () => {
    const {
      data, columnId,
    } = this.props;
    const clickData = {
      id: data.id,
      statusId: data.statusId,
      objectVersionNumber: data.objectVersionNumber,
      completed: !data.completed,
      projectId: AppState.currentMenuType.id,
    };
    boardApi.updateStatus(data.id, clickData).then((res) => {
      ScrumBoardStore.updateStatusLocal(columnId, data, res);
    }).catch((error) => {
      Choerodon.prompt(error.message, 'error');
    });
  }

  render() {
    const menu = AppState.currentMenuType;
    const { type, id: projectId, organizationId: orgId } = menu;
    const {
      data, index, isDragDisabled,
    } = this.props;
    const [disableDelete, reason] = this.getDisabled();
    return (
      <Draggable
        isDragDisabled={isDragDisabled}
        key={data.code}
        draggableId={`${data.statusId},${data.objectVersionNumber}`}
        index={index}
        type="status"
      >
        {(provided) => (
          <div
            ref={provided.innerRef}
            {...provided.draggableProps}
            {...provided.dragHandleProps}
            className={prefix}
          >
            <Permission service={['choerodon.code.project.cooperation.iteration-plan.ps.status.delete']}>
              <div className={`${prefix}-delete`}>
                {disableDelete ? (
                  <Tooltip title={disableDelete ? `${reason}，不可删除` : undefined}>
                    <Icon type="delete" style={{ fontSize: '14px', color: 'gray' }} />
                  </Tooltip>
                ) : (
                  <Button
                    size="small"
                    icon="delete"
                    disabled={disableDelete}
                    onClick={this.handleDeleteClick}
                  />
                )}
              </div>
            </Permission>
            <div
              className={`${prefix}-status`}
              style={{
                background: STATUS[data.categoryCode],
              }}
            >
              {data.status ? data.status : data.name}
            </div>
            <div className={`${prefix}-content`}>
              {data.issues ? `${data.issues.length} issues` : ''}
            </div>
            <Permission type={type} projectId={projectId} organizationId={orgId} service={['choerodon.code.project.cooperation.iteration-plan.ps.status.update']}>
              <Radio
                style={{ marginRight: 0 }}
                checked={data.completed ? data.completed : false}
                onClick={this.handleSetComplete}
              >
                设置已完成
                <Tooltip title="勾选后，卡片处于此状态的编号会显示为：#̶0̶0̶1̶，卡片状态视为已完成。" placement="topRight">
                  <Icon
                    type="help"
                    className={`${prefix}-set-complete-icon`}
                  />
                </Tooltip>
              </Radio>
            </Permission>
          </div>
        )}
      </Draggable>
    );
  }
}

export default StatusCard;
