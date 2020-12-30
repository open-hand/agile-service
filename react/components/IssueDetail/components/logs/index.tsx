import React, { useEffect, useState } from 'react';
import { observer } from 'mobx-react-lite';
import { Button } from 'choerodon-ui/pro';
import IssueLogs from '@/components/Logs';
import fieldsMap from '@/components/EditIssue/Component/DataLogFieldsMap';
import { useDetailContext } from '../../context';
import './index.less';

const Logs: React.FC = () => {
  const { store } = useDetailContext();
  const [expand, setExpand] = useState<boolean>(false);

  useEffect(() => {
    if (store.selected) {
      store.getLogs(store.selected);
    }
  }, [store, store.selected]);

  const handleExpandBtnClick = () => {
    setExpand(!expand);
  };

  const { logs, issue } = store;
  const {
    creationDate, createdBy,
    createrImageUrl, createrEmail,
    createrName, createrRealName, createrLoginName, issueTypeVO,
  } = issue;
  // @ts-ignore
  const logsWithCreate = logs.map((log) => ({ ...log, fieldName: log.field })).concat({
    email: createrEmail,
    field: issueTypeVO?.typeCode,
    imageUrl: createrImageUrl,
    name: createrName,
    realName: createrRealName,
    loginName: createrLoginName,
    lastUpdateDate: creationDate,
    lastUpdatedBy: createdBy,
    newString: 'issueNum',
    newValue: 'issueNum',
    logId: 'create',
  });

  return (
    <div className="c7n-issue-logs">
      <IssueLogs
        datalogs={logsWithCreate}
        expand={expand}
        fieldsMap={fieldsMap}
      />
      {
        logsWithCreate.length > 5 && (
          <Button
            className="c7n-issue-logs-expandBtn"
            onClick={handleExpandBtnClick}
            icon={expand ? 'baseline-arrow_drop_up icon' : 'baseline-arrow_right icon'}
          >
            {
              expand ? '折叠' : '展开'
            }
          </Button>
        )
      }
    </div>
  );
};

export default observer(Logs);
