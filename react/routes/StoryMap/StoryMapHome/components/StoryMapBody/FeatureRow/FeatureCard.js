import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { observer } from 'mobx-react';
import { Tooltip } from 'choerodon-ui';
import StatusTag from '@/components/StatusTag';
import StoryMapStore from '../../../../../../stores/project/StoryMap/StoryMapStore';
import Card from '../Card';
import './FeatureCard.less';

@observer
class FeatureCard extends Component {
  handleClick = () => {
    const { feature } = this.props;
    if (feature.issueId !== 'none') {
      StoryMapStore.setClickIssue(feature);
    }
  }

  render() {
    const { feature } = this.props;
    const {
      featureType, summary, issueId, statusVO,
    } = feature;
    const { selectedIssueMap } = StoryMapStore;
    return (
      <Card className={`c7nagile-StoryMap-FeatureCard minimapCard ${featureType || 'none'} ${statusVO && statusVO.completed ? 'completedCard' : undefined} ${selectedIssueMap.has(issueId) ? 'selected' : ''}`} onClick={this.handleClick}>
        <div className="summary">
          <Tooltip title={`${summary || '无特性'}`} getPopupContainer={(trigger) => trigger.parentNode}>
            {summary || '无特性'}
          </Tooltip>
        </div>
        {
          summary && (
          <div className="status">
            <Tooltip mouseEnterDelay={0.5} title={`状态： ${statusVO && statusVO.name}`}>
              <div>
                <StatusTag
                  data={statusVO || {}}
                />
              </div>
            </Tooltip>
          </div>
          )
        }
      </Card>
    );
  }
}

FeatureCard.propTypes = {

};

export default FeatureCard;
