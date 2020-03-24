import React, { useState } from 'react';
import { Icon } from 'choerodon-ui';
import './HistoryItem.less';

const prefix = 'c7n-agile-pi-history-item';
function HistoryItem({ data, logo }) {
  const [expand, setExpand] = useState(false);
  const { name, code, teamSprintVOS } = data;
  const renderTeam = team => (
    <div className={`${prefix}-team`}>
      <div className={`${prefix}-team-name`}>
        {`${team.projectVO.name}:  `}
      </div>
      <div className={`${prefix}-sprints`}>
        {team.sprints.map(sprint => (
          <div className={`${prefix}-sprint`}>
            {sprint.sprintName}
          </div>
        ))}
      </div>
    </div>
  );
  return (
    <div className={prefix}>
      <div className={`${prefix}-logo`}>
        <Icon type={logo} />
      </div>
      <div className={`${prefix}-line`} />
      <div className={`${prefix}-content`}>
        <div className={`${prefix}-content-header`}>
          <div className={`${prefix}-name`}>
            {name}
          </div>
          <div className={`${prefix}-date`}>
            {/* {date} */}
          </div>
        </div>
        <div className={`${prefix}-content-bottom`}>
          {(expand ? teamSprintVOS : teamSprintVOS.slice(0, 1)).map(team => renderTeam(team))}
        </div>
      </div>
      {teamSprintVOS.length > 1 && (
        <div className={`${prefix}-expand ${expand ? `${prefix}-expanded` : ''}`} onClick={() => setExpand(e => !e)}>
          <Icon type="skipped_a" />
        </div>
      )}
    </div>
  );
}
export default HistoryItem;
