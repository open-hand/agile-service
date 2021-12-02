import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { C7NFormat } from '@choerodon/master';
import { Icon } from 'choerodon-ui/pro';
import { DragDropContext, Droppable } from 'react-beautiful-dnd';
import { epicApi } from '@/api';
import BacklogStore from '../../../../stores/project/backlog/BacklogStore';
import EpicItem from './EpicItem';
import './Epic.less';
import openCreateEpic from './CreateEpic';

@observer
class Epic extends Component {
  constructor(props) {
    super(props);
    this.state = {
      addEpic: false,
    };
  }

  componentDidMount() {
    this.epicRefresh();
  }

  epicRefresh = () => {
    Promise.all([epicApi.loadEpics(), BacklogStore.axiosGetColorLookupValue()]).then(([epicList, lookupValues]) => {
      BacklogStore.initEpicList(epicList, lookupValues);
    });
  };

  /**
   *点击epicItem的事件
   *
   * @param {*} type
   * @memberof Epic
   */
  handleClickEpic = (type) => {
    const { chosenEpic } = BacklogStore;
    BacklogStore.setChosenEpic(type === chosenEpic ? 'all' : type);
    BacklogStore.axiosGetSprint().then((res) => {
      BacklogStore.setSprintData(res);
    }).catch(() => {
    });
  };

  handleOpenCreateIssue = () => {
    const { openCreateIssueModal } = this.props;
    openCreateIssueModal('BacklogEpic');
  };

  render() {
    const { draggableIds, addEpic } = this.state;
    const { refresh, issueRefresh } = this.props;
    return (
      <div className="c7n-backlog-epic">
        <div className="c7n-backlog-epicContent">
          <div className="c7n-backlog-epicTitle">
            <p style={{ fontWeight: 'bold' }}>
              <C7NFormat
                intlPrefix="agile.common"
                id="epic"
              />

            </p>
            <div className="c7n-backlog-epicRight">
              <p
                className="primary"
                style={{ cursor: 'pointer', whiteSpace: 'nowrap' }}
                role="none"
                onClick={() => {
                  openCreateEpic({
                    store: BacklogStore,
                    refresh: this.epicRefresh,
                    cantCreateEvent: this.handleOpenCreateIssue,
                    typeIdChange: (id) => {
                      BacklogStore.setDefaultTypeId(id);
                    },
                    summaryChange: (summary) => {
                      BacklogStore.setDefaultSummary(summary);
                    },
                    epicNameChange: BacklogStore.setDefaultEpicName,
                  });
                }}
              >
                <C7NFormat
                  intlPrefix="agile.common"
                  id="create.epic"
                />
              </p>
              <Icon
                type="first_page"
                role="none"
                onClick={() => {
                  BacklogStore.toggleVisible(null);
                }}
                style={{
                  cursor: 'pointer',
                  marginLeft: 6,
                }}
              />
            </div>
          </div>
          <div className="c7n-backlog-epicChoice">
            <DragDropContext
              onDragEnd={(result) => {
                const { destination, source } = result;
                if (!destination || !source) {
                  return;
                }
                const { index: destinationIndex } = destination;
                const { index: sourceIndex } = source;
                BacklogStore.moveEpic(sourceIndex, destinationIndex);
              }}
            >
              <Droppable droppableId="epic">
                {(provided, snapshot) => (
                  <div
                    ref={provided.innerRef}
                    style={{
                      background: snapshot.isDraggingOver ? '#e9e9e9' : 'white',
                      padding: 'grid',
                    }}
                  >
                    <EpicItem
                      clickEpic={this.handleClickEpic}
                      draggableIds={draggableIds}
                      refresh={refresh}
                      issueRefresh={issueRefresh}
                    />
                    {provided.placeholder}
                  </div>
                )}
              </Droppable>
            </DragDropContext>
            <div
              className="c7n-backlog-epicItems-last c7n-backlog-epicItems-last-assgin"
              style={{
                background: BacklogStore.getChosenEpic === 'unset' ? 'rgba(140, 158, 254, 0.16)' : '',
              }}
              role="none"
              onClick={() => {
                this.handleClickEpic('unset');
              }}
              onMouseEnter={(e) => {
                if (BacklogStore.isDragging) {
                  BacklogStore.toggleIssueDrag(true);
                  e.currentTarget.style.border = '2px dashed green';
                }
              }}
              onMouseLeave={(e) => {
                if (BacklogStore.isDragging) {
                  BacklogStore.toggleIssueDrag(false);
                  e.currentTarget.style.border = 'none';
                }
              }}
              onMouseUp={(e) => {
                if (BacklogStore.getIsDragging) {
                  BacklogStore.toggleIssueDrag(false);
                  e.currentTarget.style.border = 'none';
                  epicApi.addIssues(
                    0, BacklogStore.getIssueWithEpicOrVersion,
                  ).then(() => {
                    issueRefresh();
                    refresh();
                  }).catch(() => {
                    issueRefresh();
                    refresh();
                  });
                }
              }}
            >
              未指定史诗的工作项
            </div>
          </div>
        </div>
      </div>
    );
  }
}

export default Epic;
