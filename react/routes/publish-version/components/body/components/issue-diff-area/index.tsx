import React, { useEffect, useMemo, useState } from 'react';
import {
  Button, Modal, Table, Tooltip, Form, DataSet,
} from 'choerodon-ui/pro';
import classnames from 'classnames';
import { observer } from 'mobx-react-lite';
import { WSHandler } from '@choerodon/boot';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { publishVersionApi, publishVersionApiConfig, versionApi } from '@/api';
// @ts-ignore
import JSONbig from 'json-bigint';
import SelectAppService from '@/components/select/select-app-service';
import SelectGitTags from '@/components/select/select-git-tags';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import SelectPublishVersion from '@/components/select/select-publish-version';
import wsProgress from '@/components/ws-progress';
import { getProjectId } from '@/utils/common';
import styles from './index.less';
import PublishVersionSection from '../section';
import { openPreviewResultModal } from './PreviewResultModal';

const JSONbigString = JSONbig({ storeAsString: true });

function IssueDiffArea() {
  const [disablePreview, setDisablePreview] = useState(true);
  const [generateBtnLoading, setGenerateBtnLoading] = useState(false);
  const [publishVersionId, setPublishVersionId] = useState<string>();
  const { store, issueDiffDataSet } = usePublishVersionContext();
  const dependencyList = useMemo(() => (store.getDependencyList[0]?.children || []).filter((item) => item.type === 'tag'), [store.getDependencyList]);

  // const applicationId = useMemo(() => {
  //   const appServiceCode = ds.current?.get('appServiceCode');
  //   if (appServiceCode) {
  //     const newId = store.getAppServiceList.find((service) => service.code === appServiceCode)?.id;
  //     ds.current?.set('appServiceId', newId);
  //     return newId;
  //   }
  //   return appServiceCode;
  // }, [ds, ds.current?.get('appServiceCode')]);
  const handleSubmit = async () => {
    if (await issueDiffDataSet.validate()) {
      // storyTableDataSet.query();
      await publishVersionApi.comparePreviewTag(store.getCurrentData.id, issueDiffDataSet.toData());
      return true;
    }
    return false;
  };
  function handleMessage(data: any) {
    const newData = JSONbigString.parse(data);
    setGenerateBtnLoading(true);
    setDisablePreview(true);

    if (newData.action === 'done') {
      setGenerateBtnLoading(false);
      setDisablePreview(false);
    }
    console.log('newDATA', newData);
  }
  useEffect(() => {
    if (dependencyList.length === 0) {
      issueDiffDataSet.loadData([{}]);
    } else {
      issueDiffDataSet.loadData(dependencyList.map((i) => ({
        appServiceCode: i.appServiceCode,
        appServiceId: store.findAppServiceByCode(i.appServiceCode!)?.id,
        sourceTag: i.tagName,
      })));
    }
  }, [dependencyList, issueDiffDataSet, store]);
  function renderTags(record: Record) {
    const appServiceId = record.get('appServiceId');
    return [<SelectAppService request={() => publishVersionApi.loadAppServiceList(store.getCurrentData.id)} record={record} name="appServiceCode" onChange={(v) => record.set('appServiceId', v ? store.findAppServiceByCode(v)?.id : undefined).init('sourceTag', undefined).init('targetTag', undefined)} />,
      <SelectGitTags record={record} name="sourceTag" applicationId={appServiceId} key={`select-sourceTag-${appServiceId}`} />,
      <SelectGitTags record={record} name="targetTag" applicationId={appServiceId} key={`select-targetTag-${appServiceId}`} />];
  }
  function handleOpenPreview() {
    openPreviewResultModal({ tableData: [], selectIssue: (issueId) => store.selectIssue(issueId) });
  }
  return (
    <div className={styles.wrap}>
      <PublishVersionSection border={false} className={styles.section} bodyClassName={styles.body}>
        <SelectPublishVersion
          name="publishVersionId"
          labelLayout={'float' as any}
          label="对比发布版本"
          style={{ width: '4.52rem' }}
          onChange={setPublishVersionId}
        />
      </PublishVersionSection>
      <PublishVersionSection title="选择对比tag" className={styles.section} bodyClassName={styles.body} border={false}>
        <Form dataSet={issueDiffDataSet} columns={3} className={classnames(styles.form)}>
          {/* <SelectAppService name="lastAppService" /> */}
          {issueDiffDataSet.records.map((r) => renderTags(r))}

          <div className={styles.compare}>
            <WSHandler
              messageKey={`agile-preview-tag-compare-issues${getProjectId()}`}
              onMessage={handleMessage}
            >

              <Button loading={generateBtnLoading} funcType={'raised' as any} color={'primary' as any} onClick={handleSubmit}>生成预览信息</Button>
            </WSHandler>
            <Button disabled={disablePreview} funcType={'raised' as any} color={'primary' as any} onClick={handleOpenPreview}>查看结果</Button>

          </div>
        </Form>
      </PublishVersionSection>
    </div>
  );
}
export default observer(IssueDiffArea);
