import React, {
  useEffect, useMemo, useCallback, Fragment,
} from 'react';
import PropTypes from 'prop-types';
import {
  Modal, Form, DataSet, TextField, Select,
} from 'choerodon-ui/pro';
import { MAX_LENGTH_STATUS } from '@/constants/MAX_LENGTH';
import StatusTag from './StatusTag';
import DataSetFactory from './dataSet';

import './index.less';

const key = Modal.key();
const propTypes = {
  onCreate: PropTypes.func.isRequired,
};

function CreateStatus({
  modal, onCreate, 
}) {
  const dataSet = useMemo(() => new DataSet(DataSetFactory()), []);
  const handleSubmit = useCallback(async () => {
    const success = await dataSet.submit();
    if (success) {
      onCreate();
    }    
    return success;
  }, [dataSet, onCreate]);
  useEffect(() => {
    modal.handleOk(handleSubmit);
  }, [modal, handleSubmit]);
  return (
    <Fragment>
      <Form dataSet={dataSet}>
        <TextField name="name" maxLength={MAX_LENGTH_STATUS} />
        <Select
          name="categoryCode"
          optionRenderer={({ record }) => (<StatusTag data={record.toData()} />)} 
        />
      </Form>
    </Fragment>
  );
}
CreateStatus.propTypes = propTypes;
CreateStatus.open = ({
  onCreate,
}) => {
  Modal.open({
    title: '创建状态',
    key,
    drawer: true,
    style: {
      width: 380,
    },
    children: <CreateStatus onCreate={onCreate} />,
  });
};
export default CreateStatus;
