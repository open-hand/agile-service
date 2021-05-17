/* eslint-disable no-param-reassign */
import React, { Component, useEffect, useMemo } from 'react';
import {
  Modal, Form, Select, Icon, DataSet,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import {
  Content, Choerodon,
} from '@choerodon/boot';
import { find } from 'lodash';
import { getProjectId } from '@/utils/common';
import { devOpsApi } from '@/api';
import SelectApp from '@/components/CreateBranch/SelectApp';
import './LinkBranch.less';
import './commom.less';
import { IModalProps } from '@/common/types';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { Observer } from 'mobx-react';
import SelectAppService from '../select/select-app-service';
import SelectGitTags from '../select/select-git-tags';

const { Option } = Select;
interface ILinkBranchModalProps{
    issueId:string,
    onOk?:Function
}
const LinkBranch: React.FC<{ modal?: IModalProps }&ILinkBranchModalProps> = ({ modal }) => {
  const formDs = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [
      { name: 'source', label: '服务来源', type: 'string' as FieldType },
      { name: 'app', label: '应用服务', type: 'object' as FieldType },
      { name: 'applicationId', type: 'string' as FieldType, bind: 'app.id' },
      { name: 'branch', label: '分支', type: 'string' as FieldType },
    ],
    events: {
      update: ({ record, value, name }:any) => {
        if (name === 'source') {
          record.init('app', undefined);
          record.init('branch', undefined);
        } else if (name === 'app') {
          console.log('value..', value, record.getPristineValue('app'));
          // record.set('applicationId',);
          record.init('branch', undefined);
        }
      },
    },
  }), []);
  return (
    <Form dataSet={formDs}>
      <Select name="source">
        <Option value="self">本项目</Option>
        <Option value="other">其他项目</Option>
      </Select>
      <SelectAppService name="app" />
      <SelectGitTags name="branch" applicationId={formDs.current?.get('applicationId')} style={{ width: '100%' }} />
      {/* <Observer>{() => }</Observer> */}
    </Form>
  );
};
const openLinkBranchModal = (props:ILinkBranchModalProps) => {
  Modal.open({
    key: Modal.key(),
    title: '添加关联分支',
    style: {
      width: MODAL_WIDTH.middle,
    },
    drawer: true,
    children: <LinkBranch {...props} />,

  });
};
export default openLinkBranchModal;
