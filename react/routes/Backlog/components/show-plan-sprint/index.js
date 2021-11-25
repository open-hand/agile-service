import React, { useEffect } from 'react';
import { observer } from 'mobx-react-lite';
import { CheckBox } from 'choerodon-ui/pro';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import { localPageCacheStore } from '@/stores/common/LocalPageCacheStore';
import useFormatMessage from '@/hooks/useFormatMessage';

function ShowPlanSprint() {
  useEffect(() => {
    if (typeof (localPageCacheStore.getItem('backlog.show.sprint')) !== 'undefined') {
      BacklogStore.setShowPlanSprint(localPageCacheStore.getItem('backlog.show.sprint'));
    }
  }, []);
  const formatMessage = useFormatMessage('agile.backlog');
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
        {formatMessage({ id: 'show.init.sprint' })}
      </CheckBox>
    </div>
  );
}

export default observer(ShowPlanSprint);
