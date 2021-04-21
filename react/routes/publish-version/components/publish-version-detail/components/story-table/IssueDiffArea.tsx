import React, { useMemo, useState } from 'react';
import {
  Button, Modal, Table, Tooltip, Form, DataSet,
} from 'choerodon-ui/pro';
import classnames from 'classnames';
import { observer } from 'mobx-react-lite';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import UserTag from '@/components/tag/user-tag';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { publishVersionApi, publishVersionApiConfig, versionApi } from '@/api';
import renderStatus from '@/components/column-renderer/status';
import SelectAppService from '@/components/select/select-app-service';
import SelectGitTags from '@/components/select/select-git-tags';
import { useReleaseDetailContext } from '../../stores';
import styles from './IssueDiffArea.less';

function ButtonExpandCollapse({ defaultExpand, onClick }: { defaultExpand?: boolean, onClick?: (isExpand: boolean, oldIsExpand: boolean) => void | boolean }) {
  const [expand, setExpand] = useState(defaultExpand);
  return (
    <Button
      funcType={'flat' as any}
      className={styles.expand_btn}
      icon={expand ? 'expand_less' : 'expand_more'}
      onClick={() => setExpand((oldValue) => {
        const middleResult = onClick && onClick(!oldValue, !!oldValue);
        return typeof (middleResult) === 'undefined' ? !oldValue : !!middleResult;
      })}
    >
      {expand ? '收起' : '展开'}
    </Button>
  );
}
ButtonExpandCollapse.defaultProps = { defaultExpand: false, onClick: undefined };
const { Column } = Table;
function IssueDiffArea() {
  const [expand, setExpand] = useState(true);
  const { storyTableDataSet, isInProgram, store } = useReleaseDetailContext();
  const ds = useMemo(() => new DataSet({
    // autoCreate: true,
    autoQuery: true,
    paging: false,
    fields: [
      // {
      //   name: 'lastAppService', label: '选择应用服务', type: 'string' as any, required: true,
      // },
      {
        name: 'appServiceCode', label: '选择应用服务', type: 'string' as any, required: true,
      },
      {
        name: 'sourceTag', label: 'sourceTag', type: 'string' as any, required: true,
      },
      {
        name: 'targetTag', label: 'targetTag', type: 'string' as any, required: true,
      },
    ],
    transport: {
      read: () => ({ ...publishVersionApiConfig.loadCompareHistory(store.getCurrentData.id) }),
      submit: ({ data }) => ({ ...publishVersionApiConfig.compareTag(store.getCurrentData.id, data) }),
    },
  }), [store.getCurrentData.id]);
  const applicationId = useMemo(() => ds.current?.get('appServiceCode'), [ds, ds.current?.get('appServiceCode')]);
  const handleSubmit = async () => {
    if (await ds.submit()) {
      storyTableDataSet.query();
      return true;
    }
    return false;
  };
  console.log('applicationId17', applicationId);
  return (
    <div className={styles.wrap}>
      <Form dataSet={ds} columns={3} className={classnames(styles.form, { [styles.form_hidden]: !expand })}>
        {/* <SelectAppService name="lastAppService" /> */}
        <SelectAppService name="appServiceCode" valueField="id" onChange={() => ds.current?.init('sourceTag', undefined).init('targetTag', undefined)} />
        <SelectGitTags name="sourceTag" help={undefined} applicationId={applicationId} key={`select-sourceTag-${applicationId}`} />
        <SelectGitTags name="targetTag" help={undefined} applicationId={applicationId} key={`select-targetTag-${applicationId}`} />
        <div className={styles.compare} hidden={!expand}>
          <Button funcType={'raised' as any} color={'primary' as any} onClick={handleSubmit}>对比</Button>
          <span>版本对比后，自动更新issue的tag信息。</span>
        </div>
      </Form>
      <ButtonExpandCollapse defaultExpand={expand} onClick={setExpand} />
    </div>
  );
}
export default observer(IssueDiffArea);
