import React, {
  useEffect, useMemo, useCallback,
} from 'react';
import PropTypes from 'prop-types';
import { find } from 'lodash';
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

function CreateColumn({
  modal, onCreate, statusList, sequence, boardId,
}) {
  const dataSet = useMemo(() => new DataSet(DataSetFactory({ sequence, boardId })), []);
  const submit = async () => {
    const success = await dataSet.submit();
    if (success) {
      onCreate();
    }
    return success;
  };
  const handleSubmit = useCallback(async () => {
    if (await dataSet.validate()) {
      return submit();
    }
    return false;
  }, [dataSet]);
  useEffect(() => {
    modal.handleOk(handleSubmit);
  }, [modal, handleSubmit]);
  return (
    <>
      <Form dataSet={dataSet}>
        <TextField name="name" maxLength={MAX_LENGTH_STATUS} valueChangeAction="input" />
        <Select
          name="categoryCode"
          optionRenderer={({ record }) => (<StatusTag data={record.toData()} />)}
        />
      </Form>
    </>
  );
}
CreateColumn.propTypes = propTypes;
CreateColumn.open = (props) => {
  Modal.open({
    title: '创建列',
    key,
    drawer: true,
    style: {
      width: 380,
    },
    children: <CreateColumn {...props} />,
  });
};
export default CreateColumn;
