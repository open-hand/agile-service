import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Form, Icon } from 'choerodon-ui';
import { versionApi } from '@/api';
import { DragDropContext, Droppable } from 'react-beautiful-dnd';
import BacklogStore from '../../../../stores/project/backlog/BacklogStore';
import VersionItem from './VersionItem';
import './Version.less';

@observer
class Version extends Component {
  constructor(props) {
    super(props);
    this.state = {
      draggableIds: [],
    };
  }

  componentDidMount() {
    this.versionRefresh();
  }

  versionRefresh = () => {
    versionApi.loadAll().then((res) => {
      BacklogStore.setVersionData(res);
    });
  };

  /**
   *点击versionItem事件
   *
   * @param {*} type
   * @memberof Version
   */
  handleClickVersion(type) {
    const { chosenVersion } = BacklogStore;
    BacklogStore.setChosenVersion(type === chosenVersion ? 'all' : type);
    BacklogStore.axiosGetSprint().then((res) => {
      BacklogStore.setSprintData(res);
    }).catch(() => {
    });
  }

  render() {
    const {
      issueRefresh,
      refresh,
    } = this.props;
    const { draggableIds } = this.state;

    return (
      <div className="c7n-backlog-version">
        <div className="c7n-backlog-versionChoice">
          <DragDropContext
            onDragEnd={(result) => {
              const { destination, source, draggableId } = result;
              if (!destination || !source) {
                return;
              }
              const { droppableId: destinationId, index: destinationIndex } = destination;
              const { droppableId: sourceId, index: sourceIndex } = source;
              BacklogStore.moveVersion(sourceIndex, destinationIndex);
            }}
          >
            <Droppable droppableId="version" type="VERSION">
              {(provided, snapshot) => (
                <div
                  ref={provided.innerRef}
                  style={{
                    background: snapshot.isDraggingOver ? '#e9e9e9' : 'white',
                  }}
                >
                  <VersionItem
                    handleClickVersion={this.handleClickVersion}
                    refresh={refresh}
                    issueRefresh={issueRefresh}
                  />
                  {provided.placeholder}
                </div>
              )}
            </Droppable>
          </DragDropContext>
          <div
            className="c7n-backlog-versionItems"
            style={{
              background: BacklogStore.getChosenVersion === 'unset' ? 'rgba(140, 158, 254, 0.16)' : '',
            }}
            role="none"
            onClick={() => {
              this.handleClickVersion('unset');
            }}
            onMouseUp={() => {
              if (BacklogStore.getIsDragging) {
                versionApi.addIssues(
                  0, BacklogStore.getIssueWithEpicOrVersion,
                ).then((res) => {
                  issueRefresh();
                  refresh();
                }).catch(() => {
                  refresh();
                });
              }
            }}
          >
            未指定版本的工作项
          </div>
        </div>
      </div>
    );
  }
}

export default Form.create()(Version);
