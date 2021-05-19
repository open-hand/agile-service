import React, { useEffect, useCallback, useMemo } from 'react';
import {
  Modal, Form, TextField, DataSet, CheckBox,
} from 'choerodon-ui/pro';
import { IModalProps } from '@/common/types';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { personalFilterApi } from '@/api';
import { Choerodon } from '@choerodon/boot';

interface Props {
  modal?: IModalProps,
  searchVO: any,
  onOk: () => void
}
async function checkName(value: string) {
  const data: boolean = await personalFilterApi.checkName(value);
  if (data) {
    return '筛选名称重复';
  }

  return true;
}
const SaveFilterModal: React.FC<Props> = (props) => {
  const { modal, searchVO, onOk } = props;
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'filterName',
      label: '筛选名称',
      type: 'string' as FieldType,
      maxLength: 10,
      required: true,
      validator: checkName,
    }, {
      name: 'default',
      label: '设为默认',
    }],
  }), []);
  const handleSubmit = useCallback(async () => {
    if (!await dataSet.current?.validate()) {
      return false;
    }
    const value = dataSet.toData()[0] as any;
    const data = {
      default: value.default,
      name: value.filterName,
      filterJson: JSON.stringify(searchVO),
      personalFilterSearchVO: searchVO,
    };
    try {
      await personalFilterApi.create(data);
      Choerodon.prompt('保存成功');
      onOk();
      return true;
    } catch (error) {
      console.log(error);
      Choerodon.prompt('保存失败');
      return false;
    }
  }, [dataSet, onOk, searchVO]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  return (
    <Form dataSet={dataSet}>
      <TextField
        name="filterName"
        maxLength={10}
        onInput={(e) => {
          // @ts-ignore
          dataSet.current?.set('filterName', e.target.value);
          dataSet.current?.getField('filterName')?.checkValidity();
        }}
      />
      <CheckBox name="default">设为默认</CheckBox>
    </Form>
  );
};

const openSaveFilterModal = (props: Props) => {
  Modal.open({
    key: 'SaveFilterModal',
    title: '保存筛选',
    children: <SaveFilterModal {...props} />,
  });
};
export default openSaveFilterModal;
