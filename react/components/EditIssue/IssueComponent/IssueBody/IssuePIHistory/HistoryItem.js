import React, { useState } from 'react';
import { Icon } from 'choerodon-ui/pro';
import moment from 'moment';
import './HistoryItem.less';

const prefix = 'c7n-agile-pi-history-item';
function HistoryItem({ data, featureType }) {
  const [expand, setExpand] = useState(false);
  const { name, code, teamSprintVOS, fullName } = data;
  const date = data.actualStartDate || data.startDate;
  const renderTeam = (team) => (
    <div className={`${prefix}-team`}>
      <div className={`${prefix}-team-name`}>
        {`${team.projectVO.name}:  `}
      </div>
      <div className={`${prefix}-sprints`}>
        {team.sprints.map((sprint) => (
          <div className={`${prefix}-sprint`}>
            {sprint.sprintName || '无'}
          </div>
        ))}
      </div>
    </div>
  );
  return (
    <div className={prefix}>
      {featureType === 'business' ? (
        <div className={`${prefix}-logo-normal`}>
          <Icon type="characteristic" />
        </div>
      ) : (
        <div className={`${prefix}-logo`}>
          <Icon type="agile-feature" />
        </div>
      )}
      <div className={`${prefix}-line`} />
      <div className={`${prefix}-content`}>
        <div className={`${prefix}-content-header`}>
          <div className={`${prefix}-name`}>
            {fullName ?? name}
          </div>
          <div className={`${prefix}-date`}>
            {date && moment(date).format('YYYY-MM-DD')}
          </div>
        </div>
        <div className={`${prefix}-content-bottom`}>
          {(expand ? teamSprintVOS : teamSprintVOS.slice(0, 5)).map((team) => renderTeam(team))}
        </div>
      </div>
      {teamSprintVOS.length > 5 && (
        <div className={`${prefix}-expand ${expand ? `${prefix}-expanded` : ''}`} onClick={() => setExpand((e) => !e)}>
          <Icon type="skipped_a" />
        </div>
      )}
    </div>
  );
}
export default HistoryItem;
