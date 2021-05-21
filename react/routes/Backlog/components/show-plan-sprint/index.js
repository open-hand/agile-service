import React, { useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { CheckBox } from 'choerodon-ui/pro';
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
      <CheckBox
        // className="primary"
        style={{ marginLeft: 20 }}
        checked={BacklogStore.showPlanSprint}
        onChange={(checked) => {
          localPageCacheStore.setItem('backlog.show.sprint', checked);
          BacklogStore.setShowPlanSprint(checked);
        }}
      >
        显示未开始冲刺
      </CheckBox>
    </div>
  );
}

export default observer(ShowPlanSprint);
