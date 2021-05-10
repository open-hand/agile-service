import React, { useMemo, useState } from 'react';
import {
  Button, Modal, Table, Tooltip, Form, DataSet,
} from 'choerodon-ui/pro';
import classnames from 'classnames';
import { observer } from 'mobx-react-lite';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { publishVersionApi, publishVersionApiConfig, versionApi } from '@/api';
// @ts-ignore
import JSONbig from 'json-bigint';
import SelectAppService from '@/components/select/select-app-service';
import SelectGitTags from '@/components/select/select-git-tags';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import SelectPublishVersion from '@/components/select/select-publish-version';
import styles from './index.less';
import PublishVersionSection from '../section';

const JSONbigString = JSONbig({ storeAsString: true });

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
function IssueDiffArea() {
  const [expand, setExpand] = useState(true);
  const [publishVersionId, setPublishVersionId] = useState<string>();
  const { store } = usePublishVersionContext();
  const ds = useMemo(() => new DataSet({
    // autoCreate: true,
    autoQuery: true,
    paging: false,
    fields: [
      // {
      //   name: 'lastAppService', label: '选择应用服务', type: 'string' as any, required: true,
      // },
      {
        name: 'appServiceId',
      },
      {
        name: 'appServiceCode', label: '应用服务', type: 'string' as any, required: true,
      },
      {
        name: 'sourceTag', label: '当前tag', type: 'string' as any, required: true,
      },
      {
        name: 'targetTag', label: '对比tag', type: 'string' as any, required: true,
      },
    ],
    transport: {
      read: () => ({
        ...publishVersionApiConfig.loadCompareHistory(store.getCurrentData.id),
        transformResponse: (res) => {
          const data = JSONbigString.parse(res);

          return data.map((item: any) => {
            const appServiceObject = store.getAppServiceList.find((service) => service.code === item.appServiceCode) || item.appServiceCode;
            console.log('appServiceObject', appServiceObject);
            return ({
              appServiceObject, appServiceId: appServiceObject?.id, appServiceCode: item.appServiceCode, sourceTag: item.source, targetTag: item.target,
            });
          });
        },
      }),
      submit: ({ data }) => ({ ...publishVersionApiConfig.compareTag(store.getCurrentData.id, data) }),
    },
  }), [store.getAppServiceList, store.getCurrentData.id]);
  const applicationId = useMemo(() => {
    const appServiceCode = ds.current?.get('appServiceCode');
    if (appServiceCode) {
      const newId = store.getAppServiceList.find((service) => service.code === appServiceCode)?.id;
      ds.current?.set('appServiceId', newId);
      return newId;
    }
    return appServiceCode;
  }, [ds, ds.current?.get('appServiceCode')]);
  const handleSubmit = async () => {
    if (await ds.submit()) {
      // storyTableDataSet.query();
      return true;
    }
    return false;
  };
  return (
    <div className={styles.wrap}>
      <PublishVersionSection border={false} className={styles.section} bodyClassName={styles.body}>
        <SelectPublishVersion
          name="publishVersionId"
          labelLayout={'float' as any}
          required
          label="对比发布版本"
          style={{ width: '4.52rem' }}
          onChange={setPublishVersionId}
        />
      </PublishVersionSection>
      <PublishVersionSection title="选择对比tag" className={styles.section} bodyClassName={styles.body} border={false}>
        <Form dataSet={ds} columns={3} className={classnames(styles.form, { [styles.form_hidden]: !expand })}>
          {/* <SelectAppService name="lastAppService" /> */}
          <SelectAppService name="appServiceCode" onChange={() => ds.current?.init('sourceTag', undefined).init('targetTag', undefined)} />
          <SelectGitTags name="sourceTag" help={undefined} applicationId={applicationId} key={`select-sourceTag-${applicationId}`} />
          <SelectGitTags name="targetTag" help={undefined} applicationId={applicationId} key={`select-targetTag-${applicationId}`} />
          <div className={styles.compare}>
            <Button funcType={'raised' as any} color={'primary' as any} onClick={handleSubmit}>生成预览信息</Button>
            <Button funcType={'raised' as any} color={'primary' as any}>查看结果</Button>

          </div>
        </Form>
      </PublishVersionSection>
    </div>
  );
}
export default observer(IssueDiffArea);
