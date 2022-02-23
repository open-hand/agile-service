/* eslint-disable react/prop-types */
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Draggable } from 'react-beautiful-dnd';
import {
  Radio, Icon, Tooltip,
} from 'choerodon-ui';
import { find } from 'lodash';
import { stores, Permission, Choerodon } from '@choerodon/boot';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import STATUS from '@/constants/STATUS';
import './StatusCard.less';
import { boardApi } from '@/api';

const { AppState } = stores;
const prefix = 'c7n-scrumsetting-card';
@observer
class StatusCard extends Component {
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
            <Permission
              type={type}
              projectId={projectId}
              organizationId={orgId}
              service={[
                'choerodon.code.project.cooperation.iteration-plan.ps.status.update',
                'choerodon.code.project.cooperation.sprint.iteration-plan.ps.status.update',
              ]}
            >
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
