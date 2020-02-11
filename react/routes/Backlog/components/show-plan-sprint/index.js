import React from 'react';
import { observer } from 'mobx-react';
import { Checkbox } from 'choerodon-ui';
import BacklogStore from '@/stores/project/backlog/BacklogStore';


function ShowPlanSprint() {
  return (
    <Checkbox
      className="primary"
      style={{ marginLeft: 20 }}
      checked={BacklogStore.showPlanSprint}
      onChange={(e) => {
        BacklogStore.setShowPlanSprint(e.target.checked);
      }}
    >
      显示未开始冲刺
    </Checkbox>
  );
}

export default observer(ShowPlanSprint);
