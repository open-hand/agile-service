import React, {
  useCallback,
  useEffect, useMemo, useRef, useState,
} from 'react';
import {
  Button,
  DataSet, DatePicker, Form, Modal, Radio, Select, TextField, Tooltip,
} from 'choerodon-ui/pro/lib';
import { debounce, isEmpty } from 'lodash';
import classnames from 'classnames';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import SelectAppService from '@/components/select/select-app-service';
import SelectGitTags from '@/components/select/select-git-tags';
import RadioGroup from 'choerodon-ui/lib/radio/group';
// import './index.less';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import { IModalProps } from '@/common/types';
import { IAppVersionData, versionApi } from '@/api';
import { Checkbox } from 'choerodon-ui';
import SelectTeam from '@/components/select/select-team';

interface ILinkServiceProps {
    handleOk?: ((data: any) => void) | (() => Promise<any>)

}
const { Option } = Select;

const CreatePublishVersion: React.FC<{ modal?: IModalProps } & ILinkServiceProps> = ({
  modal, handleOk,
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
      { name: 'name', label: '发布版本名称', required: true },
      { name: 'actualDate', label: '实际发布时间' },
      { name: 'artifactId', label: 'artifactId' },
      { name: 'groupId', label: 'groupId' },
      { name: 'appService', label: '关联应用服务' },
      { name: 'tag', label: '关联tag' },
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
      <TextField name="name" />
      <DatePicker name="actualDate" />
      <TextField name="artifactId" />
      <TextField name="groupId" />
      <SelectAppService name="appService" onChange={setApplicationId} />
      <SelectGitTags name="tag" applicationId={applicationId} />
    </Form>
  );
};
function openCreatePublishVersionModal() {
  const key = Modal.key();
  Modal.open({
    key,
    title: '创建发布版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <CreatePublishVersion />,

  });
}
export { openCreatePublishVersionModal };
