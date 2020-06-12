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

  componentWillMount() {
    this.versionRefresh();
  }

  versionRefresh = () => {
    BacklogStore.axiosGetVersion().then((res) => {
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

    return BacklogStore.getCurrentVisible === 'version' ? (
      <div className="c7n-backlog-version">
        <div className="c7n-backlog-versionContent">
          <div className="c7n-backlog-versionTitle">
            <p style={{ fontWeight: 'bold' }}>版本</p>
            <div className="c7n-backlog-versionRight">
              <Icon
                type="first_page"
                role="none"
                style={{
                  cursor: 'pointer',
                  marginLeft: 6,
                }}
                onClick={() => {
                  BacklogStore.toggleVisible(null);
                  BacklogStore.setIsLeaveSprint(false);
                }}
              />
            </div>
          </div>
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
                    0, draggableIds,
                  ).then((res) => {
                    issueRefresh();
                    refresh();
                  }).catch(() => {
                    refresh();
                  });
                }
              }}
            >
              未指定版本的问题
            </div>
          </div>
        </div>
      </div>
    ) : null;
  }
}

export default Form.create()(Version);
