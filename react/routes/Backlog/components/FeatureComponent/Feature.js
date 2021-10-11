import React, { Component } from 'react';
import { toJS } from 'mobx';
import { observer } from 'mobx-react';
import { Select } from 'choerodon-ui/pro';
import { min } from 'lodash';
import { DragDropContext, Droppable } from 'react-beautiful-dnd';
import { featureApi, commonApi } from '@/api';
import BacklogStore from '../../../../stores/project/backlog/BacklogStore';
import FeatureItem from './FeatureItem';
import './Feature.less';

const { Option } = Select;
@observer
class Feature extends Component {
  componentDidMount() {
    this.featureRefresh(undefined, undefined);
  }

  featureRefresh = (piId, sprintId) => {
    Promise.all([featureApi.getByPiIdInSubProject(piId, sprintId), commonApi.loadLookupValue('feature_color')]).then(([featureData, featureColor]) => {
      BacklogStore.setFeatureData(featureData);
      BacklogStore.setColorLookupValue(featureColor.lookupValues);
      BacklogStore.setRandomFeatureColor(featureData, featureColor.lookupValues);
    }).catch((error3) => { });
  };

  /**
   *点击featureItem的事件
   *
   * @param {*} type
   * @memberof
   */
  handleClickFeature = (type) => {
    const { chosenFeature } = BacklogStore;
    BacklogStore.setChosenFeature(type === chosenFeature ? 'all' : type);
    BacklogStore.axiosGetSprint().then((res) => {
      BacklogStore.setSprintData(res);
    }).catch(() => {
    });
  };

  handlePiChange = (piId) => {
    BacklogStore.setSelectedPiId(piId);
    BacklogStore.setSelectedSprintId(undefined);
    this.featureRefresh(piId, undefined);
  }

  handleSprintChange = (sprintId) => {
    const { selectedPiId } = BacklogStore;
    BacklogStore.setSelectedSprintId(sprintId);
    this.featureRefresh(selectedPiId, sprintId);
  }

  render() {
    const { refresh, issueRefresh } = this.props;
    let notDoneSprintList = [];
    let selectedPi = {};
    const { notDonePiList, selectedSprintId, selectedPiId } = BacklogStore;
    if (selectedPiId) {
      selectedPi = notDonePiList.find((pi) => pi.id === selectedPiId);
      const todoPiList = notDonePiList.filter((pi) => pi.statusCode !== 'doing');
      const minPIId = min(todoPiList.map((pi) => pi.id));
      if (selectedPi && selectedPi.statusCode === 'doing') {
        notDoneSprintList = BacklogStore.sprints.filter((item) => item.statusCode !== 'done');
      } else if (selectedPiId === minPIId) {
        notDoneSprintList = BacklogStore.piMap.get(minPIId) && BacklogStore.piMap.get(minPIId).sprints;
      }
    }
    return (
      <div className="c7n-backlog-epic">
        <div className="c7n-backlog-epicChoice">
          <Select
            onChange={this.handlePiChange}
            value={toJS(selectedPiId)}
            placeholder="PI"
            style={{ marginLeft: 8, marginBottom: 6 }}
          >
            {
              [...notDonePiList, { id: '0' }].map((pi) => (
                <Option key={pi.id} value={pi.id}>
                  <div style={{ display: 'inline-flex', alignItems: 'center' }}>
                    <span>{pi.id === '0' ? '未分配PI' : pi.fullName || `${pi.code}-${pi.name}`}</span>
                    {pi.id === (BacklogStore.piInfo && BacklogStore.piInfo.id) && (
                      <div
                        style={{
                          marginLeft: '0.08rem',
                        }}
                      >
                        <span className="c7n-agile-sprintSearchSelect-option-active">当前</span>
                      </div>
                    )}
                  </div>
                </Option>
              ))
            }
          </Select>
          {!notDonePiList.find((pi) => pi.statusCode === 'doing') && !selectedPiId && (
            <p style={{
              paddingLeft: '10px',
              paddingTop: '2px',
              color: 'rgba(0, 0, 0, 0.5)',
              fontSize: '12px',
            }}
            >
              当前没有已开启的PI
            </p>
          )}
          {
            selectedPi && notDoneSprintList && notDoneSprintList.length > 0 && (
              <Select
                style={{ marginTop: 6, marginBottom: 8, marginLeft: 8 }}
                key="sprintSelect"
                placeholder="冲刺"
                dropdownMatchSelectWidth={false}
                value={toJS(selectedSprintId)}
                onChange={this.handleSprintChange}
              >
                {
                  (notDoneSprintList || []).map((item) => (
                    <Option key={item.sprintId} value={item.sprintId} title={item.sprintName}>
                      <div style={{ display: 'inline-flex', alignItems: 'center' }}>
                        {item.sprintName}
                        {
                          item.statusCode === 'started' && (
                            <div className="c7n-agile-sprintSearchSelect-option-active">活跃</div>
                          )
                        }
                      </div>
                    </Option>
                  ))
                }
              </Select>
            )
          }
          <DragDropContext
            onDragEnd={(result) => {
              const { destination, source } = result;
              if (!destination || !source) {
                return;
              }
              const { index: destinationIndex } = destination;
              const { index: sourceIndex } = source;
              BacklogStore.moveFeature(sourceIndex, destinationIndex);
            }}
          >
            <Droppable droppableId="feature">
              {(provided, snapshot) => (
                <div
                  ref={provided.innerRef}
                  style={{
                    background: snapshot.isDraggingOver ? '#e9e9e9' : 'white',
                    padding: 'grid',
                  }}
                >
                  <FeatureItem
                    clickFeature={this.handleClickFeature}
                    refresh={refresh}
                    issueRefresh={issueRefresh}
                  />
                  {provided.placeholder}
                </div>
              )}
            </Droppable>
          </DragDropContext>
          <div
            className="c7n-backlog-epicItems-last"
            style={{
              background: BacklogStore.getChosenFeature === 'unset' ? 'rgba(140, 158, 254, 0.16)' : '',
            }}
            role="none"
            onClick={() => {
              this.handleClickFeature('unset');
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
                featureApi.addIssues(
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
            未指定特性的工作项
          </div>
        </div>
      </div>
    );
  }
}

export default Feature;
