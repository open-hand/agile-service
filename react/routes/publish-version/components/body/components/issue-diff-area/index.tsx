import React, {
  useEffect, useState,
} from 'react';
import {
  Button, Form,
} from 'choerodon-ui/pro';
import classnames from 'classnames';
import { uniqBy } from 'lodash';
import { observer } from 'mobx-react-lite';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { publishVersionApi } from '@/api';
// @ts-ignore
import JSONbig from 'json-bigint';
import SelectAppService from '@/components/select/select-app-service';
import SelectGitTags from '@/components/select/select-git-tags';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import SelectPublishVersion from '@/components/select/select-publish-version';
import styles from './index.less';
import PublishVersionSection from '../section';
import { openPreviewResultModal } from './PreviewResultModal';
import { requestPreviewData } from './utils';

const JSONbigString = JSONbig({ storeAsString: true });

function IssueDiffArea() {
  const [tableData, setTableData] = useState<any[] | false | undefined>();
  const [generateBtnLoading, setGenerateBtnLoading] = useState(false);
  const [publishVersionId, setPublishVersionId] = useState<string>();
  const { store, issueDiffDataSet } = usePublishVersionContext();
  const dependencyList = store.getDependencyList;

  const handleSubmit = async () => {
    if (await issueDiffDataSet.validate()) {
      setTableData(false);
      setGenerateBtnLoading(true);
      const newData = await requestPreviewData(store.getCurrentData.id, issueDiffDataSet.toData());
      setGenerateBtnLoading(false);

      setTableData(newData);
      return true;
    }
    return false;
  };
  function handleMessage(data: any) {
    const newData = JSONbigString.parse(data);
    const oneTableData = newData.data ? JSONbigString.parse(newData.data) : [];
    console.log('newDATA', newData);
    setGenerateBtnLoading(true);
    setTableData((oldData) => (!oldData ? [] : oldData.concat(...oneTableData)));

    if (newData.action === 'done') {
      setGenerateBtnLoading(false);
      // setTableData(false);
    }
  }
  useEffect(() => {
    publishVersionApi.loadCompareHistory(store.getCurrentData.id); // .....
  }, []);
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
  function handleChangeIssueTag(action: 'add' | 'update') {
    publishVersionApi.compareTag(store.getCurrentData.id, issueDiffDataSet.toData(), action);
  }
  function handleOpenPreview() {
    const uniqTableData = uniqBy((tableData || []), (item) => item.issueId);
    openPreviewResultModal({
      publishVersionId: store.getCurrentData.id, onChangeIssueTag: handleChangeIssueTag, tableData: uniqTableData, selectIssue: (issueId) => store.selectIssue(issueId),
    });
  }
  return (
    <div className={styles.wrap}>
      <PublishVersionSection border={false} className={classnames(styles.section, styles.top_form)} bodyClassName={styles.body}>
        <Form>
          <SelectPublishVersion
            name="publishVersionId"
            label="对比发布版本"
            style={{ width: '4.52rem' }}
            onChange={setPublishVersionId}
          />
        </Form>
      </PublishVersionSection>
      <PublishVersionSection title="选择对比tag" className={styles.section} bodyClassName={styles.body} border={false}>
        <Form dataSet={issueDiffDataSet} columns={3} className={classnames(styles.form)}>
          {/* <SelectAppService name="lastAppService" /> */}
          {issueDiffDataSet.records.map((r) => renderTags(r))}

          <div className={styles.compare}>
            <Button loading={generateBtnLoading} funcType={'raised' as any} color={'primary' as any} onClick={handleSubmit}>生成预览信息</Button>
            <Button disabled={!tableData || generateBtnLoading} funcType={'raised' as any} color={'primary' as any} onClick={handleOpenPreview}>查看结果</Button>
          </div>
        </Form>
      </PublishVersionSection>
    </div>
  );
}
export default observer(IssueDiffArea);
