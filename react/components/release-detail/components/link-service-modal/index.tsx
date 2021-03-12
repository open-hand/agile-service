import React, {
  useEffect, useMemo, useRef, useState,
} from 'react';
import {
  Button,
  DataSet, Form, Modal, Radio, Select, TextField,
} from 'choerodon-ui/pro/lib';
import classnames from 'classnames';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import SelectAppService from '@/components/select/select-app-service';
import SelectGitTags from '@/components/select/select-git-tags';
import RadioGroup from 'choerodon-ui/lib/radio/group';
import './index.less';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { Observer, observer } from 'mobx-react-lite';

interface ILinkServiceProps {
  handleOk?: (data: any) => void
}
const { Option } = Select;
const SelectVersion: React.FC<Partial<SelectProps>> = observer(({ ...otherProps }) => {
  const inputRef = useRef<TextField>(null);
  const selectRef = useRef<Select>(null);
  const [inputVisible, setInputVisible] = useState<boolean>();
  function handleSelect(newValue: string) {
    selectRef.current?.choose(new Record({ name: '1111111', value: newValue }));
  }
  const [options, setOptions] = useState<Array<any>>(() => new Array(19).fill(0).map((item, index) => ({ value: String(index), name: `${1.2 + index}b` })));
  return (
    <Select
      ref={selectRef}
      trigger={['click'] as any}
      popupContent={({ dataSet }: { dataSet: DataSet }) => {
        console.log('props', dataSet.length);
        return (
          <div>
            <ul className="c7n-pro-select-dropdown-menu  c7n-pro-select-dropdown-menu-root c7n-pro-select-dropdown-menu-vertical">
              {dataSet.map((i, index) => (
                <li
                  role="none"
                  className={classnames('c7n-pro-select-dropdown-menu-item', { 'c7n-pro-select-dropdown-menu-item-selected': selectRef.current?.getValue() === String(index) })}
                  onClick={() => handleSelect(i.get('value'))}
                >
                  {i.get('name')}
                </li>
              ))}
            </ul>
            <div role="none" className="c7n-agile-release-detail-select-footer" onFocus={(e) => { e.stopPropagation(); console.log('onFocus footer....'); }}>
              {
                inputVisible ? (
                  <div className="c7n-agile-release-detail-select-footer-input">
                    <TextField ref={inputRef} onClick={(e) => { e.stopPropagation(); console.log('click...'); inputRef.current?.focus(); }} autoFocus style={{ width: '100%' }} />
                    <Button
                      color={'primary' as any}
                      onClick={(e) => {
                        e.stopPropagation();
                        // handleLoadMore();
                        console.log('primary', inputRef.current?.getValue());
                        setOptions((oldOptions) => [...oldOptions, { name: inputRef.current?.getValue(), value: '009' }]);
                        handleSelect(String(dataSet.length));
                        // dataSet.create({ name: inputRef.current?.getValue(), value: '009' });
                        setInputVisible(false);

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

          </div>
        );
      }}
      {...otherProps}
    >
      {options.flatMap((i, index) => <Option value={String(index)}>{`${1.2 + index}b`}</Option>)}
    </Select>
  );
});
const LinkService: React.FC = () => {
  const [applicationId, setApplicationId] = useState<string>();
  const [versionType, setVersionType] = useState<string>('tag');

  const ds = useMemo(() => new DataSet({
    autoQuery: false,
    autoCreate: true,
    paging: false,
    // data: [
    //   { appService: '应用1', alias: undefined },
    // ],
    fields: [
      { name: 'appService', label: '选择应用服务', required: true },
      { name: 'tag', label: '选择tag', dynamicProps: { required: ({ record }) => record.get('change') === 'tag' } },
      {
        name: 'version', label: '选择版本', textField: 'name', valueField: 'value', dynamicProps: { required: ({ record }) => record.get('change') === 'version' },
      },
      { name: 'change', defaultValue: 'tag' },
    ],
  }), []);
  useEffect(() => {
    ds.current?.init(versionType, undefined);
  }, [ds, versionType]);
  return (
    <Form dataSet={ds}>
      <SelectAppService name="appService" onChange={setApplicationId} />
      <RadioGroup>
        <Radio name="change" value="tag" onChange={setVersionType}>选择Tag</Radio>
        <Radio name="change" value="version" onChange={setVersionType}>选择版本</Radio>
      </RadioGroup>
      {versionType === 'tag' ? <SelectGitTags name="tag" applicationId={applicationId} key={`git-tags-${applicationId}`} />
        : <SelectVersion name="version" disabled={!applicationId} />}

    </Form>
  );
};
function openLinkServiceModal(props?: ILinkServiceProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '关联应用版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <LinkService />,

  });
}
export { openLinkServiceModal };
