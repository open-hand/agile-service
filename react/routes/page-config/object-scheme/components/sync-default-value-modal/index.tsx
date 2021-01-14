import { pageConfigApi, pageConfigApiConfig } from '@/api';
import { IModalProps } from '@/common/types';
import TextEditToggle from '@/components/TextEditTogglePro';
import renderEditor from '@/routes/page-config/page-issue-type/components/sort-table/renderEditor';
import {
  Form, DataSet, Modal, Select,
} from 'choerodon-ui/pro/lib';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import React, { useCallback, useEffect, useState } from 'react';

interface Props {
  prefixCls:string
  record: Record
  text: string
  defaultValue: any
  extraConfig?: boolean
  options: Array<{ name: string, code: string }>
  modal?: IModalProps
}
const SyncDefaultValueEditForm: React.FC<Props> = ({
  text, record, defaultValue, options, extraConfig, modal, prefixCls,
}) => {
  const [showText, setShowText] = useState<string>(text);
  const [value, setValue] = useState(defaultValue);
  function handleSubmit(data: any) {
    setValue(data);
    setShowText(options.filter((option) => data.includes(option.code)).map((item) => item.name).toString());
  }

  const handleOk = useCallback(async () => {
    await pageConfigApi.syncDefaultValue(record.get('id'), String(value), extraConfig);
    return true;
  }, [extraConfig, record, value]);
  useEffect(() => {
    modal?.handleOk(handleOk);
  }, [handleOk, modal]);
  return (
    <div className={`${prefixCls}-detail-sync-form`}>
      <span>是否将默认值同步至:</span>
      <TextEditToggle
        initValue={value}
        onSubmit={handleSubmit}
        editor={() => (
          <Select name="syncIssueType" required style={{ minWidth: 280 }} multiple validationRenderer={() => '请选择同步的问题类型'}>
            {options.map((option) => <Select.Option value={option.code}>{option.name}</Select.Option>)}
          </Select>
        )}
      >
        {showText}
      </TextEditToggle>
    </div>
  );
};
export default SyncDefaultValueEditForm;
