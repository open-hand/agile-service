import React, { useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { Checkbox } from 'choerodon-ui';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';

function ShowPlanSprint() {
  useEffect(() => {
    if (typeof (localPageCacheStore.getItem('backlog.show.sprint')) !== 'undefined') {
      BacklogStore.setShowPlanSprint(localPageCacheStore.getItem('backlog.show.sprint'));
    }
  }, []);
  return (
    <div>
      <Checkbox
        // className="primary"
        style={{ marginLeft: 20, color: 'black' }}
        checked={BacklogStore.showPlanSprint}
        onChange={(e) => {
          localPageCacheStore.setItem('backlog.show.sprint', e.target.checked);
          BacklogStore.setShowPlanSprint(e.target.checked);
        }}
      >
        显示未开始冲刺
      </Checkbox>
    </div>
  );
}

export default observer(ShowPlanSprint);
