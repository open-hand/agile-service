import React, {
  useMemo, useEffect, useCallback, useState,
} from 'react';
import {
  Select, DataSet, Button, Icon,
} from 'choerodon-ui/pro';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import {
  observer, useComputed, useObservable,
} from 'mobx-react-lite';
import { devOpsApi } from '@/api';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { getProjectId } from '@/utils/common';
import { randomString } from '@/utils/random';

import SelectGitTags from '../select-git-tags';
import './index.less';
import SelectSubProject from '../select-sub-project';

const prefixCls = 'c7n-agile-select-multi-service-tag';

export interface IMultiServiceTagItemProps {
  tagName: string
  appServiceCode: string
  projectId: string

}

export interface IMultiServiceTagProps {
  onOK?: (data: IMultiServiceTagItemProps[]) => void
  onCancel?: () => void
  data: Array<Pick<IMultiServiceTagItemProps, 'appServiceCode'> & { tagName: string[] }>
  projectId?: string
  mode?: 'program' | 'project'
}
const SelectAppService: React.FC<Partial<SelectProps> & { loadData: (record: Record) => any[] }> = observer(({ record, loadData, ...otherProps }) => {
  // const options = useObserver(() => (loadData(record!) as any[]).map((item) => <Option value={item.id}>{renderService(item)}</Option>));
  const optionData = useComputed(() => (loadData(record!) as any[]).map((item) => ({ ...item, meaning: `${item.name}(${item.code})`, value: item.code })), []);
  const optionsDs = useMemo(() => new DataSet({
    data: optionData,
    paging: false,
    autoCreate: false,
    autoQuery: false,
  }), [optionData]);
  return (
    <Select record={record} {...otherProps} options={optionsDs} />
  );
});
const MultiServiceTag: React.FC<IMultiServiceTagProps> = ({
  onOK, onCancel, projectId, data, mode = 'project',
}) => {
  const appServerListMaps = useObservable(new Map<string, any[] | false>());
  const [loadTask, setLoadTask] = useState<string[]>([]);

  const ds = useMemo(() => new DataSet({
    autoCreate: false,
    autoQuery: false,
    paging: false,
    dataToJSON: 'normal' as any,
    data,
    fields: [
      { name: 'projectId', label: '子项目' },
      {
        name: 'appService', label: '应用服务', textField: 'meaning', valueField: 'code', type: 'object' as any, ignore: 'always' as any,
      },
      { name: 'appServiceId', label: '应用服务', bind: 'appService.id' },
      { name: 'appServiceCode', label: '应用服务', bind: 'appService.code' },
      { name: 'tagName', label: 'Tag' },
    ],
  }), [data]);

  useEffect(() => {
    function loadAppServiceData(codeProjectId: string) {
      devOpsApi.project(codeProjectId).loadActiveService().then((res: any) => {
        Array.isArray(res) && appServerListMaps.set(String(codeProjectId), res);
        // setAppServiceList(res);
        ds.filter((r) => String(r.get('projectId')) === String(codeProjectId)).forEach((record) => {
          const appServiceCode = record.get('appServiceCode');
          const appServiceId = record.get('appServiceId');

          if (appServiceCode && !appServiceId) {
            const findItem = res?.find((item: any) => item.code === appServiceCode);
            record.init('appService', findItem ? { ...findItem, meaning: `${findItem.name}(${findItem.code})` } : {
              appServiceCode, meaning: appServiceCode, value: appServiceCode, projectId: String(codeProjectId),
            });
          }
        });
      });
    }
    for (let i = 0; i < loadTask.length; i += 1) {
      const task = loadTask[i];
      if (!appServerListMaps.has(task)) {
        appServerListMaps.set(task, false);
        loadAppServiceData(task);
      }
    }
  }, [appServerListMaps, ds, loadTask]);

  const getCurrentProjectId = useCallback((record: Record) => {
    if (mode === 'program') {
      return record.get('projectId');
    }
    const currentProjectId = projectId || record.get('projectId') || getProjectId();
    return currentProjectId;
  }, [mode, projectId]);
  const getAppServerList = useCallback((record: Record) => {
    const currentProjectId = getCurrentProjectId(record);

    !appServerListMaps.has(String(currentProjectId)) && currentProjectId && setLoadTask((tasks) => [...tasks, currentProjectId]);
    const appServiceList = appServerListMaps.get(String(currentProjectId));
    return appServiceList || [];
  }, [appServerListMaps, getCurrentProjectId]);

  const handleCreate = useCallback(() => {
    ds.create();
  }, [ds]);
  function handleResetData(e: any) {
    ds.reset();
    e.stopPropagation();
  }
  function handleCancel(e: any) {
    onCancel && onCancel();
    handleResetData(e);
  }
  function handleSave(e: any) {
    const saveData: IMultiServiceTagItemProps[] = [];
    ds.toJSONData().forEach((item: any) => {
      saveData.push(...(item.tagName || []).map((tag: string) => ({ tagName: tag, appServiceCode: item.appServiceCode, projectId: item.projectId || projectId || getProjectId() })));
    });
    onOK && onOK(saveData);
    handleResetData(e);
  }

  useEffect(() => {
    // 若无数据则创建
    data && data.length === 0 && handleCreate();
  }, [data, handleCreate]);

  const loadAppServiceData = useCallback((record: Record) => {
    const currentProjectId = getCurrentProjectId(record);
    const applicationCodes = ds.filter((r) => String(r.get('projectId')) === String(currentProjectId)).map((r) => r.get('appServiceCode'));
    const appServiceList = getAppServerList(record);
    const recordAppServiceList = appServiceList?.filter((appService) => record.get('appServiceCode') === appService.code || !applicationCodes.includes(appService.code));
    return recordAppServiceList || [];
  }, [ds, getAppServerList, getCurrentProjectId]);
  const componentId = useMemo(() => `select-multi-service-tag-${randomString(5)}`, []);
  const disabledAddButton = useComputed(() => ds.filter((record) => !(record.get('appServiceCode') && record.get('tagName'))).length > 0, [ds]);

  return (
    <div className={prefixCls} role="none" id={componentId} onClick={(e) => e.stopPropagation()} onMouseDown={(e) => e.stopPropagation()}>
      <div className={`${prefixCls}-content`}>
        {ds.map((record) => {
          const currentProjectId = getCurrentProjectId(record);
          const appServiceId = record.get('appServiceId') || (appServerListMaps.get(String(currentProjectId)) || [])?.find((item) => item.code === record.get('appServiceCode'))?.id;

          return (
            <div className={`${prefixCls}-content-form`}>
              {mode === 'program' ? (
                <SelectSubProject
                  record={record}
                  name="projectId"
                  label="请选择子项目"
                  placeholder="请选择子项目"
                  className={`${prefixCls}-content-item`}
                  onChange={(val) => {
                    record.init('appService', undefined).init('tagName', undefined);
                  }}
                  getPopupContainer={() => document.getElementById(componentId) as any}
                />
              ) : null}
              <SelectAppService
                name="appService"
                record={record}
                disabled={!currentProjectId}
                label="请选择应用服务"
                placeholder="请选择应用服务"
                loadData={loadAppServiceData}
                onChange={(val) => {
                  mode !== 'program' && record.init('projectId', projectId || getProjectId());
                  record.init('tagName', undefined);
                }}
                className={`${prefixCls}-content-item`}
                getPopupContainer={() => document.getElementById(componentId) as any}
              />
              <SelectGitTags
                applicationId={appServiceId}
                name="tagName"
                multiple
                help={undefined}
                projectId={currentProjectId}
                disabled={!appServiceId}
                placeholder="请选择Tag"
                label="请选择Tag"
                className={`${prefixCls}-content-item`}
                record={record}
                getPopupContainer={() => document.getElementById(componentId) as any}
              />

              {ds.length > 1 && <Icon type="delete_sweep-o" className={`${prefixCls}-content-del`} onClick={() => { ds.delete(record, false); }} />}
            </div>
          );
        })}
        <Button className={`${prefixCls}-content-add`} style={{ border: 'none' }} icon="add" disabled={disabledAddButton} funcType={'flat' as any} color={'primary' as any} onClick={handleCreate}>添加Tag</Button>

      </div>
      <div className={`${prefixCls}-footer`}>
        <Button funcType={'raised' as any} color={'primary' as any} onClick={handleSave} disabled={disabledAddButton && ds.length <= 1}>确定</Button>
        <Button funcType={'raised' as any} onClick={handleCancel}>取消</Button>

      </div>
    </div>

  );
};

export default observer(MultiServiceTag);
