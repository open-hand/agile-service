import React, { Component, createRef } from 'react';
import PropTypes from 'prop-types';
import { Tooltip, Icon } from 'choerodon-ui';
import { observer } from 'mobx-react';
import StatusTag from '@/components/StatusTag';
import EpicDrag from './EpicDrag';
import StoryMapStore from '../../../../../stores/project/StoryMap/StoryMapStore';
import Card from './Card';
import EditEpic from './EditEpic';
import './EpicCard.less';

@observer
class EpicCard extends Component {
  constructor(props) {
    super(props);
    this.state = {
      editing: false,
    };
    this.editEpicRef = createRef();
  }

  handleClick = () => {
    const { epic } = this.props;
    if (epic.issueId) {
      StoryMapStore.setClickIssue({ ...epic, epicId: epic.issueId });
    }
  }

  setZIndex = () => {
    this.container.style.zIndex = 9999;
  }

  resetZIndex = () => {
    this.container.style.zIndex = 'unset';
  }

  saveRef = (ref) => {
    const { connectDragSource, saveRef } = this.props;
    connectDragSource(ref);
    if (saveRef) {
      saveRef(ref);
    }
    this.container = ref;
  }

  handleEditEpic(e) {
    e.stopPropagation();
    this.setState({
      editing: true,
    });
  }

  setEditing(editing) {
    this.setState({
      editing,
    });
  }

  handleClickOutside() {
    if (this.editEpicRef && this.editEpicRef.current?.handleEditEpic) {
      this.editEpicRef.current?.handleEditEpic(this.editEpicRef.current?.value);
    }
  }

  render() {
    const {
      epic, subIssueNum, onMouseDown,
    } = this.props;
    const {
      issueId, epicName, statusVO,
    } = epic;
    const { selectedIssueMap } = StoryMapStore;
    const { editing } = this.state;
    return (
      <>
        {
        !editing ? (
          <Card
            className={`c7nagile-StoryMap-EpicCard minimapCard ${issueId ? '' : 'none'} ${statusVO && statusVO.completed ? 'completedCard' : undefined}  ${selectedIssueMap.has(issueId) ? 'selected' : ''}`}
            style={{ display: 'flex' }}
            onClick={this.handleClick}
            saveRef={this.saveRef}
            onMouseDown={onMouseDown}
          >
            <div className="top">
              <div className="summary">
                <Tooltip title={`${epicName || '无史诗'}`} getPopupContainer={() => document.getElementsByClassName('minimap-container-scroll')[0]}>
                  {`${epicName || '无史诗'}`}
                </Tooltip>
              </div>
              <div style={{ marginLeft: 5 }}>{`(${subIssueNum})`}</div>
              <div className="c7nagile-StoryMap-EpicCard-editIcon" role="none" onClick={this.handleEditEpic.bind(this)}>
                <Icon type="mode_edit" />
              </div>
            </div>
            {
              epicName && (
                <div className="status">
                  <StatusTag
                    data={statusVO || {}}
                  />
                </div>
              )
            }
          </Card>
        ) : (
          <EditEpic
            epic={epic}
            // eslint-disable-next-line react/jsx-no-bind
            setEditing={this.setEditing.bind(this)}
            handleClickOutside={this.handleClickOutside}
            editEpicRef={this.editEpicRef}
          />
        )
      }
      </>
    );
  }
}

EpicCard.propTypes = {

};

export default EpicDrag(EpicCard);
