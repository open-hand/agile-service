import React, {
  useCallback,
  useEffect, useMemo, useRef, useState,
} from 'react';
import {
  Button,
  DataSet, Form, Modal, Radio, Select, TextField, Tooltip,
} from 'choerodon-ui/pro/lib';
import { debounce, isEmpty } from 'lodash';
import classnames from 'classnames';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import SelectAppService from '@/components/select/select-app-service';
import SelectGitTags from '@/components/select/select-git-tags';
import RadioGroup from 'choerodon-ui/lib/radio/group';
import './index.less';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import { IModalProps } from '@/common/types';
import { IAppVersionData, versionApi } from '@/api';
import { Checkbox } from 'choerodon-ui';
import SelectTeam from '@/components/select/select-team';

interface ILinkServiceProps {
  handleOk?: ((data: any) => void) | (() => Promise<any>)
  versionId: string
  programMode?: boolean
}
const { Option } = Select;
interface SelectVersionProps extends Partial<SelectProps> {
  versionId: string
  serviceCode?: string
  subProjectId?: string
  programMode?: boolean
}
const SelectVersion: React.FC<SelectVersionProps> = observer(({
  versionId, serviceCode, subProjectId, multiple, programMode, ...otherProps
}) => {
  const inputRef = useRef<TextField>(null);
  const selectRef = useRef<Select>(null);
  const [inputVisible, setInputVisible] = useState<boolean>();
  function handleSelect(record: Record) {
    if (multiple && selectRef.current?.isSelected(record)) {
      selectRef.current?.unChoose(record);
    } else {
      selectRef.current?.choose(record);
    }
  }
  const loadData = useCallback(() => {
    console.log('versionId.', versionId, serviceCode);
    if (programMode) {
      versionId && subProjectId && versionApi.loadProgramAppService(versionId, subProjectId).then((res: any) => {
        setOptions(res);
      });
    } else {
      versionId && serviceCode && versionApi.loadAvailableAppVersionList(versionId, serviceCode).then((res: any) => {
        console.log('res', res.content);
        setOptions(res.content);
      });
    }
  }, [programMode, serviceCode, subProjectId, versionId]);
  useEffect(() => {
    loadData();
  }, [loadData]);
  const [options, setOptions] = useState<Array<any>>([]);
  function handleCheckVersion(name: string) {
    return versionApi.checkAppVersion({ artifactId: serviceCode!, serviceCode: serviceCode!, version: name });
  }
  // async function handleInput(value: string) {

  // }
  const handleInput = debounce((v) => {
    if (!isEmpty(v)) {
      inputRef.current?.validate(v);
    }
  }, 350);
  async function handleValidator(value?: string) {
    if (isEmpty(value)) {
      return '请输入版本名称';
    }
    const res = await handleCheckVersion(value!);
    return res ? '版本名称重复' : true;
  }

  return (
    <Select
      ref={selectRef}
      trigger={['click'] as any}
      popupContent={({ dataSet }: { dataSet: DataSet }) => {
        console.log('props', dataSet.length);
        return (
          <div>
            {multiple ? (
              <div className="c7n-pro-select-select-all-none">
                <span role="none" onClick={() => selectRef.current?.chooseAll()}>全选</span>
                <span role="none" onClick={() => selectRef.current?.chooseRe()}>反选</span>
                <span role="none" onClick={() => selectRef.current?.unChooseAll()}>无</span>
              </div>
            ) : null}
            <ul className="c7n-pro-select-dropdown-menu  c7n-pro-select-dropdown-menu-root c7n-pro-select-dropdown-menu-vertical">
              {dataSet.length > 0 ? dataSet.map((i) => (
                <li
                  role="none"
                  className={classnames('c7n-pro-select-dropdown-menu-item', { 'c7n-pro-select-dropdown-menu-item-selected': selectRef.current?.getValue() === i.get('value') })}
                  onClick={() => handleSelect(i)}
                >
                  {multiple ? <Checkbox checked={selectRef.current?.isSelected(i)} /> : null}
                  <Tooltip title={i.get('name')}>
                    {i.get('name')}
                  </Tooltip>
                </li>
              )) : <li className="c7n-pro-select-dropdown-menu-item c7n-pro-select-dropdown-menu-item-disabled">无匹配结果。</li>}
            </ul>
            {!programMode ? (
              <div role="none" className="c7n-agile-publish-version-detail-select-footer">
                {
                  inputVisible ? (
                    <div className="c7n-agile-publish-version-detail-select-footer-input">
                      <TextField ref={inputRef} onInput={(e: any) => handleInput(e.target.value)} validator={handleValidator} onClick={(e) => { e.stopPropagation(); inputRef.current?.focus(); }} autoFocus style={{ width: '100%' }} />
                      <Button
                        color={'primary' as any}
                        onClick={async (e) => {
                          e.stopPropagation();
                          // handleLoadMore();
                          const versionName = inputRef.current?.getValue();
                          if (await inputRef.current?.validate(versionName) && serviceCode) {
                            const newVersion: IAppVersionData = await versionApi.createAppVersion({ artifactId: serviceCode!, serviceCode, version: versionName });
                            const newOption = { ...newVersion, name: `${newVersion.artifactId}/${newVersion.versionAlias || newVersion.version || ''}`, value: newVersion.id! };
                            setOptions((oldOptions) => [...oldOptions, newOption]);
                            handleSelect(new Record(newOption));
                            // dataSet.create({ name: inputRef.current?.getValue(), value: '009' });
                            setInputVisible(false);
                          }

                          // dataSet.unshift(new Record({ name: inputRef.current?.getValue(), value: `0001${inputRef.current?.getValue()}` }));
                        }}
                      >
                        保存
                      </Button>

                      <Button onClick={(e) => {
                        e.stopPropagation();
                        // handleLoadMore();
                        setInputVisible(false);
                      }}
                      >
                        取消
                      </Button>
                    </div>
                  ) : (
                    <Button
                      onClick={(e) => {
                        e.stopPropagation();
                        // handleLoadMore();
                        setInputVisible(true);
                      }}
                      color={'primary' as any}
                      style={{ margin: '-4px -12px', width: 'calc(100% + 24px)' }}
                    >
                      创建版本
                    </Button>
                  )
                }
              </div>
            )
              : null}
          </div>
        );
      }}
      {...otherProps}
    >
      {options.flatMap((i) => <Option value={i.id}>{i.name}</Option>)}
    </Select>
  );
});
const LinkService: React.FC<{ modal?: IModalProps } & ILinkServiceProps> = ({
  modal, versionId, programMode, handleOk,
}) => {
  const [applicationId, setApplicationId] = useState<string>();
  const [versionType, setVersionType] = useState<string>('version');

  const ds = useMemo(() => new DataSet({
    autoQuery: false,
    autoCreate: true,
    paging: false,
    // data: [
    //   { appService: '应用1', alias: undefined },
    // ],
    fields: [
      { name: 'appService', label: '选择应用服务', required: !programMode },
      { name: 'subProject', label: '选择子项目', required: !!programMode },

      { name: 'tag', label: '选择tag', dynamicProps: { required: ({ record }) => record.get('change') === 'tag' } },
      {
        name: 'version', label: '选择版本', multiple: true, textField: 'name', valueField: 'value', dynamicProps: { required: ({ record }) => record.get('change') === 'version' },
      },
      { name: 'change', defaultValue: 'version' },
    ],
  }), []);
  useEffect(() => {
    ds.current?.init(versionType, undefined);
  }, [ds, versionType]);
  const handleSubmit = useCallback(async () => {
    if (!await ds.current?.validate()) {
      return false;
    }
    const data = ds.current?.toData();
    const result = handleOk && await handleOk(data);
    return typeof (result) !== 'undefined' ? result : true;
  }, [ds, handleOk]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  return (
    <Form dataSet={ds}>
      {programMode
        ? <SelectTeam name="subProject" onChange={setApplicationId} />
        : <SelectAppService name="appService" onChange={setApplicationId} />}

      <RadioGroup>
        <Radio name="change" value="version" onChange={setVersionType}>选择版本</Radio>

        <Radio name="change" value="tag" onChange={setVersionType}>选择Tag</Radio>
      </RadioGroup>
      {versionType === 'tag' ? <SelectGitTags name="tag" applicationId={applicationId} key={`git-tags-${applicationId}`} />
        : <SelectVersion name="version" disabled={!applicationId} maxTagCount={5} versionId={versionId} serviceCode={applicationId} subProjectId={applicationId} programMode={programMode} multiple />}

    </Form>
  );
};
function openLinkServiceModal(props: ILinkServiceProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '关联应用版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <LinkService {...props} />,

  });
}
export { openLinkServiceModal };
