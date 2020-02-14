import React, {
  useEffect, useMemo, useCallback, Fragment,
} from 'react';
import PropTypes from 'prop-types';
import { find } from 'lodash';
import {
  Modal, Form, DataSet, TextField, Select,
} from 'choerodon-ui/pro';
import { MAX_LENGTH_STATUS } from '@/constants/MAX_LENGTH';
import StatusTag from '../create-status/StatusTag';
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
    const data = dataSet.toData()[0];
    const existStatus = find(statusList, { name: data.name });
    if (existStatus) {
      const button = await Modal.confirm({
        title: '警告',
        children: `已存在状态“${existStatus.name}”，如果创建该列，不会创建同名状态`,
      });
      if (button === 'ok') {
        return submit();
      } else {
        return false;
      }
    } else {
      return submit();
    }
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
