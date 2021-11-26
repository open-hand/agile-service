import React, {
  useEffect, useMemo, useCallback,
} from 'react';
import {
  Modal, Form, DataSet, Select,
} from 'choerodon-ui/pro';
import { C7NFormat } from '@choerodon/master';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { statusTransformApi, IStatusCirculation } from '@/api';
import './index.less';
import { getIsOrganization } from '@/utils/common';

const key = Modal.key();
interface Props {
  onSubmit: Function
  modal?: any
  issueTypeId: string
  statusList: IStatusCirculation[]
}
const SetDefaultStatus: React.FC<Props> = ({
  modal, onSubmit, issueTypeId, statusList,
}) => {
  const isOrganization = getIsOrganization();
  const dataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      {
        name: 'status',
        type: 'object' as FieldType,
        label: '状态名称',
        required: true,
        textField: 'name',
        valueField: 'id',
        options: new DataSet({
          data: statusList,
          paging: false,
        }),
      },
    ],
  }), []);

  const handleSubmit = useCallback(async () => {
    if (await dataSet.validate()) {
      const status = dataSet.current?.get('status') as IStatusCirculation;
      const { id, stateMachineId } = status;
      await statusTransformApi[isOrganization ? 'orgSetDefaultStatus' : 'setDefaultStatus'](issueTypeId, id, stateMachineId);
      onSubmit();
      modal.close();
    }
    return false;
  }, [dataSet, isOrganization, issueTypeId, modal, onSubmit]);
  useEffect(() => {
    modal.handleOk(handleSubmit);
  }, [modal, handleSubmit]);

  return (
    <>
      <Form dataSet={dataSet}>
        <Select name="status" />
      </Form>
    </>
  );
};
const openSetDefaultStatus = (props: Omit<Props, 'modal'>) => {
  Modal.open({
    title: <C7NFormat
      intlPrefix="agile.stateMachine"
      id="flow.init.state"
    />,
    key,
    children: <SetDefaultStatus {...props} />,
  });
};
export default openSetDefaultStatus;
