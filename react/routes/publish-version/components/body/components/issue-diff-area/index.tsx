import React, {
  useCallback,
  useEffect, useMemo, useState,
} from 'react';
import {
  Button, Form, Select, Tooltip,
} from 'choerodon-ui/pro';
import classnames from 'classnames';
import { sortBy } from 'lodash';
import {
  observer, useComputed, useObservable, useObserver,
} from 'mobx-react-lite';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { publishVersionApi } from '@/api';
// @ts-ignore
import JSONbig from 'json-bigint';
import OriginSelectAppService from '@/components/select/select-app-service';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';

import SelectGitTags from '@/components/select/select-git-tags';
import { usePublishVersionContext } from '@/routes/publish-version/stores';
import SelectPublishVersion from '@/components/select/select-publish-version';
import { useMap } from 'ahooks';
import { IPublishVersionMenuDiffConfigProps } from '@/routes/publish-version';
import moment from 'moment';
import styles from './index.less';
import PublishVersionSection from '../section';
import { openPreviewResultModal } from './PreviewResultModal';
import { requestPreviewData } from './utils';

const { Option } = Select;
const JSONbigString = JSONbig({ storeAsString: true });
function SelectAppService({ request, record, ...otherProps }: Partial<SelectProps> & { request: Function }) {
  const renderService = (appService: any) => {
    if (appService) {
      return (
        <Tooltip title={appService.code}>
          <div style={{ display: 'inline-block' }}>
            {`${appService.name}(${appService.code})`}
          </div>
        </Tooltip>
      );
    }
    return null;
  };
  // const options = useObservable((request(record) || []) as any[]);
  const options = useObserver(() => (request(record) as any[]).map((item) => <Option value={item.code}>{renderService(item)}</Option>));
  return (
    <Select record={record} {...otherProps}>
      {options}
      {/* {options.map((item) => <Option value={item.code}>{renderService(item)}</Option>)} */}
    </Select>
  );
}
interface IssueDiffAreaBaseProps {
  topFormContent: React.ReactNode,
  bottomFormContent: React.ReactNode,
  bottomFormProps: { columns: number }
  menuDiffConfig?: IPublishVersionMenuDiffConfigProps
}
export function IssueDiffAreaBase({
  topFormContent, bottomFormContent, bottomFormProps, menuDiffConfig,
}: IssueDiffAreaBaseProps) {
  const tableDataMap = useMemo(() => new Map<string, any>(), []);
  const [tableData, setTableData] = useState<any[] | false | undefined>();
  const [generateBtnLoading, setGenerateBtnLoading] = useState(false);
  const { store, issueDiffDataSet } = usePublishVersionContext();
  useEffect(() => {
    setGenerateBtnLoading(false);
    setTableData(false);
  }, [store.getCurrentData.id]);
  function handleRequest(data: any) {
    return publishVersionApi.comparePreviewTag(store.getCurrentData.id, data);
  }
  const handleSubmit = async () => {
    if (await issueDiffDataSet.validate()) {
      setTableData(false);
      setGenerateBtnLoading(true);
      const newData = await requestPreviewData(issueDiffDataSet.toData().filter((item: any) => item.appServiceCode), menuDiffConfig?.onCompareTagRequest || handleRequest);
      setGenerateBtnLoading(false);
      newData.forEach((item) => {
        if (tableDataMap.has(item.issueId)) {
          const issue = tableDataMap.get(item.issueId);
          tableDataMap.set(item.issueId, { ...issue, tags: [...issue.tags, ...item.tags] });
        } else {
          tableDataMap.set(item.issueId, item);
        }
      });
      setTableData(newData);
      return true;
    }
    return false;
  };
  function handleChangeIssueTag(action: 'add' | 'update') {
    menuDiffConfig?.onChangeIssueTag ? menuDiffConfig?.onChangeIssueTag(issueDiffDataSet, action)
      : publishVersionApi.compareTag(store.getCurrentData.id, issueDiffDataSet.toData(), action);
  }
  function handleOpenPreview() {
    const uniqTableData = sortBy([...tableDataMap.values()], (item) => (!item.creationDate ? 0 : moment(item.creationDate).unix())); // uniqBy((tableData || []), (item) => item.issueId);
    console.log('uniqTableData', uniqTableData);
    openPreviewResultModal({
      onChangeIssueTag: handleChangeIssueTag, tableData: uniqTableData, handleOk: () => store.setCurrentMenu('info'), applyType: (store as any).applyType,
    });
  }
  return (
    <div className={styles.wrap}>
      <PublishVersionSection border={false} className={classnames(styles.section, styles.top_form)} bodyClassName={styles.body}>
        <Form>
          {topFormContent}
        </Form>
      </PublishVersionSection>
      <PublishVersionSection title="选择对比tag" className={styles.section} bodyClassName={styles.body} border={false}>
        <Form dataSet={issueDiffDataSet} className={classnames(styles.form)} {...bottomFormProps}>
          {bottomFormContent}

        </Form>

      </PublishVersionSection>
      <div className={styles.compare}>
        <Button loading={generateBtnLoading} funcType={'raised' as any} color={'primary' as any} onClick={handleSubmit}>生成预览信息</Button>
        <Button disabled={!tableData || generateBtnLoading} funcType={'raised' as any} color={'primary' as any} onClick={handleOpenPreview}>查看结果</Button>
      </div>
    </div>
  );
}
IssueDiffAreaBase.defaultProps = {
  menuDiffConfig: undefined,
};
function IssueDiffArea() {
  const [publishVersionId, setPublishVersionId] = useState<string>();
  const [appServerList, setAppServerList] = useState<any[]>([]);

  const { store, issueDiffDataSet } = usePublishVersionContext();
  const dependencyList = store.getDependencyList;

  useEffect(() => {
    publishVersionId && issueDiffDataSet.length > 0 && publishVersionApi.loadCompareHistory(publishVersionId).then((res: any) => {
      res.map((item: any) => {
        const findRecord = issueDiffDataSet.find((record) => record.get('appServiceCode') === item.appServiceCode);
        findRecord?.set('targetTag', item.source);
        return false;
      });
    }); // .....
  }, [issueDiffDataSet, issueDiffDataSet.length, publishVersionId]);
  useEffect(() => {
    const availableDependencyList = dependencyList.filter((i) => i.type === 'tag' || (!i.appService && i.tagName));
    if (availableDependencyList.length === 0) {
      issueDiffDataSet.loadData([{}]);
    } else {
      issueDiffDataSet.loadData(availableDependencyList.map((i) => ({
        appServiceCode: i.appServiceCode,
        appServiceId: store.findAppServiceByCode(i.appServiceCode!)?.id,
        sourceTag: i.tagName,
      })));
    }
  }, [dependencyList, issueDiffDataSet, store]);

  const loadAppServiceData = useCallback((record: Record) => {
    const applicationIds = issueDiffDataSet.map((r) => r.get('appServiceId'));
    console.log('appServerList', applicationIds, appServerList);
    return appServerList.filter((appService) => record.get('appServiceId') === appService.id || !applicationIds.includes(appService.id));
  }, [appServerList, issueDiffDataSet]);
  useEffect(() => {
    publishVersionApi.loadAppServiceList(store.getCurrentData.id).then((res: any) => {
      setAppServerList(res);
    });
  }, [store.getCurrentData.id]);
  function renderTags(record: Record) {
    const appServiceId = record.get('appServiceId');
    return [<SelectAppService request={loadAppServiceData} record={record} name="appServiceCode" onChange={(v) => record.set('appServiceId', v ? store.findAppServiceByCode(v)?.id : undefined).init('sourceTag', undefined).init('targetTag', undefined)} />,
      <SelectGitTags record={record} name="sourceTag" applicationId={appServiceId} />,
      <SelectGitTags record={record} name="targetTag" applicationId={appServiceId} />];
  }
  return (
    <IssueDiffAreaBase
      topFormContent={(
        <SelectPublishVersion
          name="publishVersionId"
          label="对比发布版本"
          afterLoad={(data) => data.filter((i) => i.id !== store.getCurrentData.id)}
          style={{ width: '4.52rem' }}
          onChange={setPublishVersionId}
        />
      )}
      bottomFormContent={issueDiffDataSet.records.map((r) => renderTags(r))}
      bottomFormProps={{ columns: 3 }}
    />
  );
}
export default observer(IssueDiffArea);
