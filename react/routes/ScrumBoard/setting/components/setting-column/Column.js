import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Draggable } from 'react-beautiful-dnd';
import { stores, Permission, Choerodon } from '@choerodon/boot';
import { Icon, Input } from 'choerodon-ui';
import { Modal } from 'choerodon-ui/pro';
import './Column.less';
import ScrumBoardStore from '@/stores/project/scrumBoard/ScrumBoardStore';
import TextEditToggle from '@/components/TextEditToggle';
import { boardColumnApi } from '@/api';
import StatusList from './StatusList';

const { AppState } = stores;
const { Text, Edit } = TextEditToggle;
@observer
class Column extends Component {
  handleDeleteColumn = () => {
    const {
      data, refresh,
    } = this.props;
    Modal.open({
      title: '删除列',
      children: '确定要删除该列？',
      onOk: async () => boardColumnApi.delete(data.columnId).then(() => {
        refresh();
      }).catch((err) => {
      }),
    });
  }

  updateColumnMaxMin = (type, value) => {
    const { data: propData, refresh } = this.props;
    const maxNum = type === 'maxNum' ? value : propData.maxNum;
    const minNum = type === 'minNum' ? value : propData.minNum;
    const data = {
      boardId: ScrumBoardStore.getSelectedBoard,
      columnId: propData.columnId,
      objectVersionNumber: propData.objectVersionNumber,
      projectId: AppState.currentMenuType.id,
      maxNum,
      minNum,
    };
    boardColumnApi.updateMaxMinNum(propData.columnId, data).then((res) => {
      const { failed } = res;
      if (!failed) {
        Choerodon.prompt('设置成功');
        refresh();
      } else if (res.code === 'error.num.minNumCannotUpToMaxNum') {
        Choerodon.prompt('最小值应该小于最大值，最大值应该大于最小值，设置失败');
      } else {
        Choerodon.prompt('设置失败');
      }
    }).catch((error) => {
      // Choerodon.prompt(error);
    });
  }

  handleSaveColumnName = (name) => {
    const { data: propData, index } = this.props;
    const data = {
      columnId: propData.columnId,
      objectVersionNumber: propData.objectVersionNumber,
      name,
      projectId: AppState.currentMenuType.id,
      boardId: ScrumBoardStore.getSelectedBoard,
    };
    boardColumnApi.update(propData.columnId, ScrumBoardStore.getSelectedBoard, data).then((res) => {
      const originData = ScrumBoardStore.getBoardData;
      originData[index].objectVersionNumber = res.objectVersionNumber;
      originData[index].name = res.name;
      ScrumBoardStore.setBoardData(originData);
    }).catch((error) => {
    });
  }

  renderColumnName = () => {
    const {
      data,
    } = this.props;
    return (
      <div className="c7n-scrumsetting-columnStatus">
        <Permission
          service={[
            'choerodon.code.project.cooperation.iteration-plan.ps.column',
            'choerodon.code.project.cooperation.sprint.iteration-plan.ps.column',
          ]}
          noAccessChildren={data.name}
        >
          <TextEditToggle
            formKey="name"
            onSubmit={this.handleSaveColumnName}
            originData={data.name}
          >
            <Text>
              {(text) => text}
            </Text>
            <Edit>
              <Input
                autoFocus
              />
            </Edit>
          </TextEditToggle>
        </Permission>
      </div>
    );
  }

  render() {
    const menu = AppState.currentMenuType;
    const {
      data, index, isDragDisabled,
    } = this.props;
    const { type, id: projectId, organizationId: orgId } = menu;
    const deleteAble = ScrumBoardStore.getColumnDeleteAble(data.columnId);
    return (
      <Draggable
        isDragDisabled={isDragDisabled}
        key={data.columnId}
        index={index}
        draggableId={JSON.stringify({
          columnId: data.columnId,
          objectVersionNumber: data.objectVersionNumber,
        })}
        type="columndrop"
      >
        {(provided) => (
          <div
            className="c7n-scrumsetting-column"
            ref={provided.innerRef}
            {...provided.draggableProps}
          >
            <div className="c7n-scrumsetting-columnContent">
              <div className="c7n-scrumsetting-columnTop">
                <div>
                  <div
                    className="c7n-scrumsetting-icons"
                  >
                    <Icon
                      type="open_with"
                      style={{
                        cursor: 'move',
                        display: isDragDisabled && 'none',
                      }}
                      {...provided.dragHandleProps}
                    />
                    {deleteAble && (
                      <Permission
                        service={[
                          'choerodon.code.project.cooperation.iteration-plan.ps.column.create',
                          'choerodon.code.project.cooperation.sprint.iteration-plan.ps.column.create',
                        ]}
                      >
                        <Icon
                          type="delete_sweep-o"
                          style={{
                            cursor: 'pointer',
                            display: isDragDisabled && 'none',
                          }}
                          role="none"
                          onClick={this.handleDeleteColumn}
                        />
                      </Permission>
                    )}
                  </div>
                </div>
                {this.renderColumnName()}
                <div
                  className="c7n-scrumsetting-columnBottom"
                  style={{
                    borderBottom: data.color ? `3px solid ${data.color}` : '3px solid rgba(0,0,0,0.26)',
                  }}
                >
                  <div
                    style={{
                      display: ScrumBoardStore.getCurrentConstraint === 'constraint_none' ? 'none' : 'flex',
                      justifyContent: 'space-between',
                      flexWrap: 'wrap',
                    }}
                  >
                    <Permission
                      service={[
                        'choerodon.code.project.cooperation.iteration-plan.ps.column',
                        'choerodon.code.project.cooperation.sprint.iteration-plan.ps.column',
                      ]}
                      noAccessChildren={(
                        <span
                          style={{ minWidth: '110px' }}
                        >
                          {'最大值：'}
                          {typeof data.maxNum === 'number' ? data.maxNum : '没有最大'}
                        </span>
                      )}
                    >
                      <TextEditToggle
                        formKey="name"
                        onSubmit={(value) => {
                          this.updateColumnMaxMin('maxNum', value);
                        }}
                        originData={data.maxNum}
                      >
                        <Text>
                          {(text) => (
                            <span
                              style={{ cursor: 'pointer', minWidth: '110px' }}
                            >
                              {'最大值：'}
                              {typeof text === 'number' ? text : '没有最大'}
                            </span>
                          )}
                        </Text>
                        <Edit>
                          <Input
                            autoFocus
                          />
                        </Edit>
                      </TextEditToggle>
                    </Permission>
                    <Permission
                      service={[
                        'choerodon.code.project.cooperation.iteration-plan.ps.column',
                        'choerodon.code.project.cooperation.sprint.iteration-plan.ps.column',
                      ]}
                      noAccessChildren={(
                        <span
                          style={{ minWidth: '110px' }}
                        >
                          {'最小值：'}
                          {typeof data.minNum === 'number' ? data.minNum : '没有最小'}
                        </span>
                      )}
                    >
                      <TextEditToggle
                        formKey="name"
                        onSubmit={(value) => {
                          this.updateColumnMaxMin('minNum', value);
                        }}
                        originData={data.minNum}
                      >
                        <Text>
                          {(text) => (
                            <span
                              style={{ cursor: 'pointer', minWidth: '110px' }}
                            >
                              {'最小值：'}
                              {typeof text === 'number' ? text : '没有最小'}
                            </span>
                          )}
                        </Text>
                        <Edit>
                          <Input
                            autoFocus
                          />
                        </Edit>
                      </TextEditToggle>
                    </Permission>
                  </div>
                </div>
              </div>
              <Permission
                service={[
                  'choerodon.code.project.cooperation.iteration-plan.ps.movetocolomn',
                  'choerodon.code.project.cooperation.sprint.iteration-plan.ps.movetocolomn',
                ]}
                noAccessChildren={(
                  <StatusList data={data} isDragDisabled />
                )}
              >
                <StatusList data={data} />
              </Permission>
            </div>
          </div>
        )}
      </Draggable>
    );
  }
}

export default Column;
