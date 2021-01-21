import React, { useMemo, useEffect, useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import {
  Modal, DataSet, Form, Select,
} from 'choerodon-ui/pro';
import { IModalProps } from '@/common/types';

interface Props {
  modal?: IModalProps,
}

const LinkType: React.FC<Props> = ({ modal }) => {
  const optionDataSet: DataSet = useMemo(() => new DataSet({
    fields: [{
      name: 'typeIds',
      textField: 'name',
      valueField: 'id',
    }],
  }), []);

  const linkDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'typeIds',
      textField: 'name',
      valueField: 'id',
      required: true,
      multiple: true,
      options: optionDataSet,
      label: '请选择引用的问题类型',
    }],
  }), [optionDataSet]);

  useEffect(() => {

  }, []);

  const handleSubmit = useCallback(async () => {
    const validate = await linkDataSet.validate();
    if (validate) {
      console.log(linkDataSet.current?.data);
      return true;
    }
    return false;
  }, [linkDataSet]);

  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  return (
    <Form dataSet={linkDataSet}>
      <Select name="typeIds" />
    </Form>
  );
};

const ObserverLinkType = observer(LinkType);

const openLink = () => {
  Modal.open({
    drawer: true,
    style: {
      width: 480,
    },
    key: 'link',
    title: '引用问题类型',
    children: <ObserverLinkType />,
  });
};

export default openLink;
