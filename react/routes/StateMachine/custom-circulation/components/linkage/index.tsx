import React, {
  useMemo, useEffect, useCallback, useState,
} from 'react';
import {
  Form, DataSet, Button, Select, Row, Col, CheckBox,
  Icon,
} from 'choerodon-ui/pro';

import { observer } from 'mobx-react-lite';
import { has as hasInject, mount as mountInject } from '@choerodon/inject';
import useIssueTypes from '@/hooks/data/useIssueTypes';
import SelectStatus from '@/components/select/select-status';
import { issueLinkTypeApi, statusTransformApi } from '@/api';
import { OldLoading as Loading } from '@/components/Loading';
import useFields from '@/routes/Issue/components/BatchModal/useFields';
import { getIsOrganization } from '@/utils/common';
import { IIssueType } from '@/common/types';
import HostPick from '@/components/host-pick';
import styles from './index.less';

interface IParentIssueStatusSetting {
  id: string
  issueTypeId: string
  parentIssueStatusSetting: string
  parentIssueTypeId: string
  projectId: number
  statusId: string
  type?: 'anyone_transfer' | 'all_transfer',
}

interface ILinkIssueStatusSetting {
  linkTypeId: string
  linkIssueTypeId: string
  linkIssueStatusId: string
  linkIssueStatus: any
  triggered: boolean
}

interface ILinkType {
  linkTypeId: string
  linkName: string
}

const { Option } = Select;
const linkTypeHasIssueTypeMap = new Map([]); // 存储关联类型都选择了哪些工作项类型，避免重复选择
const Linkage = ({
  // @ts-ignore
  modal, record, selectedType, customCirculationDataSet, linkageType, selectedTypeName, selectedTypeCode,
}) => {
  const isOrganization = getIsOrganization();
  const { data: issueTypes } = useIssueTypes({ typeCode: ['story', 'task', 'bug'] });
  const [fields, Field] = useFields();
  const [linkFields, LinkField] = useFields();
  const [loading, setLoading] = useState(false);
  const [activeKey, setActiveKey] = useState<'subIssue' | 'linkIssue'>(linkageType[0]);
  const [linkTypes, setLinkTypes] = useState<ILinkType[]>([]);
  const [linkIssueStatusSettings, setLinkIssueStatusSettings] = useState<ILinkIssueStatusSetting[]>([]);

  useEffect(() => {
    if (linkageType.includes('linkIssue')) {
      issueLinkTypeApi.getAll().then((res: { content: ILinkType[]}) => {
        setLinkTypes(res.content || []);
        (res.content || []).forEach((item) => {
          linkTypeHasIssueTypeMap.set(item.linkTypeId.toString(), []);
        });
      });
    }
  }, [linkageType]);

  const transferType = useMemo(() => {
    const statusName = record.get('name');
    return ({
      all: {
        key: 'all_transfer',
        text: `全部${selectedTypeName}流转到${statusName}`,
      },
      anyone: {
        key: 'anyone_transfer',
        text: `任一${selectedTypeName}流转到${statusName}`,
      },
    });
  }, [record, selectedTypeName]);

  const linkageDataSet = useMemo(() => new DataSet({
    autoCreate: true,
  }), []);
  const linkIssueLinkageDataSet = useMemo(() => new DataSet({
    autoCreate: true,
  }), []);
  const getFieldValue = useCallback((ds, name) => {
    const { current } = ds;
    if (current) {
      return current.get(name);
    }
    return '';
  }, []);
  const getField = useCallback((ds, name) => ds.current?.getField(name), []);

  const addField = useCallback((ds, name, props, defaultValue?: any) => {
    ds?.current?.addField(name, { ...props, name });
    if (defaultValue) {
      ds?.current?.set(name, defaultValue);
    }
  }, []);

  const addFieldRule = useCallback((key) => {
    addField(linkageDataSet, `${key}-type`, {
      required: true,
    });
    addField(linkageDataSet, `${key}-status`, {
      required: true,
    });
    addField(linkageDataSet, `${key}-transfer`, {
      required: true,
    }, transferType.all.key);
  }, [addField, linkageDataSet]);

  const linkIssueAddFieldRule = useCallback((key) => {
    addField(linkIssueLinkageDataSet, `${key}-linkTypeId`, {
      required: true,
    });
    addField(linkIssueLinkageDataSet, `${key}-linkIssueTypeId`, {
      required: true,
    });
    addField(linkIssueLinkageDataSet, `${key}-linkIssueStatusId`, {
      required: true,
    });
    addField(linkIssueLinkageDataSet, `${key}-triggered`, {
    });
  }, [addField, linkIssueLinkageDataSet]);

  const setFieldValue = useCallback((ds: DataSet, name: string, value: any) => {
    const { current } = ds;
    if (current) {
      current.set(name, value);
    }
  }, []);
  const removeField = useCallback((ds: DataSet, name: string) => {
    ds.fields.delete(name);
    ds.current?.fields.delete(name);
  }, []);
  useEffect(() => {
    const getLinkages = async () => {
      setLoading(true);
      try {
        if (linkageType.includes('subIssue')) {
          const res: IParentIssueStatusSetting[] = await statusTransformApi.getLinkage(selectedType, record.get('id'));
          const initFields = linkageType.length > 1 && !res.length ? [] : Field.init(new Array(Math.max(res.length, 1)).fill({}));
          initFields.forEach((item: { key: number }, i: number) => {
            addFieldRule(item.key);
            if (res.length > 0) {
              setFieldValue(linkageDataSet, `${item.key}-type`, res[i]?.parentIssueTypeId);
              setFieldValue(linkageDataSet, `${item.key}-status`, res[i]?.parentIssueStatusSetting);
              setFieldValue(linkageDataSet, `${item.key}-transfer`, res[i]?.type || transferType.all.key);
            }
          });
        }
        if (linkageType.includes('linkIssue')) {
          const res: ILinkIssueStatusSetting[] = await statusTransformApi.getLinkIssueLinkage(selectedType, record.get('id'));
          setLinkIssueStatusSettings(res || []);
          const initFields = linkageType.length > 1 && !res.length ? [] : LinkField.init(new Array(Math.max(res.length, 1)).fill({}));
          initFields.forEach((item: { key: number }, i: number) => {
            linkIssueAddFieldRule(item.key);
            if (res.length) {
              if (res[i]?.linkTypeId && res[i]?.linkIssueTypeId) {
                (linkTypeHasIssueTypeMap.get(res[i].linkTypeId.toString()) as string[]).push(res[i].linkIssueTypeId);
              }
              setFieldValue(linkIssueLinkageDataSet, `${item.key}-linkTypeId`, res[i]?.linkTypeId);
              setFieldValue(linkIssueLinkageDataSet, `${item.key}-linkIssueTypeId`, res[i]?.linkIssueTypeId);
              setFieldValue(linkIssueLinkageDataSet, `${item.key}-linkIssueStatusId`, res[i]?.linkIssueStatusId);
              setFieldValue(linkIssueLinkageDataSet, `${item.key}-triggered`, res[i]?.triggered);
            }
          });
        }
        setLoading(false);
      } catch (err) {
        setLoading(false);
      }
    };
    getLinkages();
  }, [Field, LinkField, addFieldRule, linkIssueAddFieldRule, linkIssueLinkageDataSet, linkageDataSet, linkageType, record, selectedType, setFieldValue]);
  useEffect(() => {
    const handleOk = async () => {
      const linkageValidate = await linkageDataSet.validate();
      const linkIssueLinkageValidate = await linkIssueLinkageDataSet.validate();
      if (linkageValidate && linkIssueLinkageValidate) {
        if (linkageType.includes('subIssue')) {
          const data: any = linkageDataSet.current?.toData();
          const updateData: any[] = [];
          fields.forEach((f: any) => {
            const { key } = f;
            updateData.push({
              parentIssueTypeId: data[`${key}-type`],
              parentIssueStatusSetting: data[`${key}-status`],
              type: data[`${key}-transfer`],
            });
          });
          // @ts-ignore
          await statusTransformApi[isOrganization ? 'orgUpdateLinkage' : 'updateLinkage'](selectedType, record.get('id'), record.get('objectVersionNumber'), updateData);
        }
        if (linkageType.includes('linkIssue')) {
          const data: any = linkIssueLinkageDataSet.current?.toData();
          const updateData: any[] = [];
          linkFields.forEach((f: any) => {
            const { key } = f;
            updateData.push({
              linkTypeId: data[`${key}-linkTypeId`],
              linkIssueTypeId: data[`${key}-linkIssueTypeId`],
              linkIssueStatusId: data[`${key}-linkIssueStatusId`],
              triggered: data[`${key}-triggered`],
            });
          });
          // @ts-ignore
          await statusTransformApi.updateLinkIssueLinkage(selectedType, record.get('id'), updateData);
        }
        customCirculationDataSet.query(customCirculationDataSet.currentPage);
        return true;
      }
      if (!linkageValidate) {
        if (linkageType.length === 2 && activeKey === 'linkIssue') {
          setActiveKey('subIssue');
        }
        return false;
      }
      if (!linkIssueLinkageValidate) {
        if (linkageType.length === 2 && activeKey === 'subIssue') {
          setActiveKey('linkIssue');
        }
        return false;
      }
      return false;
    };
    if (modal) {
      modal.handleOk(handleOk);
    }
  }, [activeKey, customCirculationDataSet, fields, isOrganization, linkFields, linkIssueLinkageDataSet, linkageDataSet, linkageType, modal, record, selectedType]);
  const selectedIssueTypes: string[] = (() => {
    const data: any = linkageDataSet.toData()[0];
    return Object.keys(data).reduce((result: string[], key) => {
      const [k, code] = key.split('-');
      if (code === 'type') {
        result.push(data[key]);
      }
      return result;
    }, []);
  })();

  const handleActiveKeyChange = useCallback((key) => {
    setActiveKey(key);
  }, []);

  return (
    <div className={styles.linkage}>
      <Loading loading={loading} />
      {
        linkageType.length === 1 && (
          <div className={styles.tip}>{linkageType[0] === 'subIssue' ? `当工作项流转到${record.get('name')}后，关联的父任务状态设置。` : `当工作项流转到${record.get('name')}后，关联的工作项满足下列设置，将自动流转到指定状态。`}</div>
        )
      }
      {
        linkageType.length === 2 && (
          <div className={styles.typePick}>
            <HostPick
              defaultActiveKey={activeKey}
              onChange={handleActiveKeyChange}
              hostTabKeys={[{
                key: 'subIssue',
                text: '父子级联动',
              }, {
                key: 'linkIssue',
                text: '关联工作项联动',
              }]}
            />
            <div className={styles.linkageTip}>{activeKey === 'subIssue' ? `当工作项流转到${record.get('name')}后，关联的父任务状态设置。` : `当工作项流转到${record.get('name')}后，关联的工作项满足下列设置，将自动流转到指定状态。`}</div>
          </div>
        )
      }
      <div>
        {
          linkageType.includes('subIssue') && activeKey === 'subIssue' && (
            <Form dataSet={linkageDataSet}>
              {fields.map((f: any) => {
                const { key, id } = f;
                const typeName = `${key}-type`;
                const statusName = `${key}-status`;
                const transferName = `${key}-transfer`;
                const issueTypeId = getFieldValue(linkageDataSet, typeName);
                return (
                  <Row key={key} gutter={20} type="flex" align="middle">
                    {hasInject('agilePro:transferSelectBox') ? (
                      <Col span={20}>
                        {mountInject('agilePro:transferSelectBox', { transferType, name: transferName })}
                      </Col>
                    ) : null}
                    <Col span={11}>
                      <Select
                        label="父任务类型"
                        name={typeName}
                        onChange={() => {
                          getField(linkageDataSet, statusName)?.reset();
                          linkageDataSet.current?.init(statusName, undefined);
                        }}
                      >
                        {
                          // @ts-ignore
                        issueTypes?.filter((type) => (selectedTypeCode === 'bug' ? type.typeCode !== 'bug' : true)).filter((type: IIssueType) => (issueTypeId && type.id === issueTypeId) || !selectedIssueTypes.includes(type.id)).map((type: IIssueType) => (
                          <Option value={type.id}>
                            {type.name}
                          </Option>
                        ))
                        }
                      </Select>
                    </Col>
                    <Col span={11} key={id}>
                      <SelectStatus
                        label="指定状态"
                        key={`${key}-${issueTypeId}`}
                        name={statusName}
                        issueTypeId={issueTypeId}
                        isOrganization={isOrganization}
                      />
                    </Col>
                    <Col span={2}>
                      <Icon
                        type="delete_sweep-o"
                        style={{
                          color: 'var(--primary-color)',
                          cursor: 'pointer',
                        }}
                        onClick={() => {
                          Field.remove(key);
                          removeField(linkageDataSet, typeName);
                          removeField(linkageDataSet, statusName);
                        }}
                      />
                    </Col>
                  </Row>
                );
              })}
              <div>
                <Button
                  style={{ marginTop: -4 }}
                  icon="playlist_add"
                  onClick={() => {
                    const newKey = Field.add();
                    addFieldRule(newKey);
                  }}
                >
                  添加联动
                </Button>
              </div>
            </Form>
          )
        }
        {
          linkageType.includes('linkIssue') && activeKey === 'linkIssue' && (
            <Form dataSet={linkIssueLinkageDataSet}>
              {linkFields.map((f: any) => {
                const { key, id } = f;
                const linkTypeName = `${key}-linkTypeId`;
                const typeName = `${key}-linkIssueTypeId`;
                const statusName = `${key}-linkIssueStatusId`;
                const triggeredName = `${key}-triggered`;
                const linkTypeId = getFieldValue(linkIssueLinkageDataSet, linkTypeName);
                const issueTypeId = getFieldValue(linkIssueLinkageDataSet, typeName);
                const statusId = getFieldValue(linkIssueLinkageDataSet, statusName);
                const extraStatus = selectedType && issueTypeId && linkTypeId ? linkIssueStatusSettings.find((item: ILinkIssueStatusSetting) => item.linkTypeId === linkTypeId && item.linkIssueTypeId === issueTypeId)?.linkIssueStatus : undefined;
                return (
                  <>
                    <Row key={key} gutter={20} type="flex" align="middle">
                      <Col span={8}>
                        <Select
                          label="关联类型"
                          name={linkTypeName}
                          onChange={(value, oldValue) => {
                          getField(linkIssueLinkageDataSet, typeName)?.reset();
                          getField(linkIssueLinkageDataSet, statusName)?.reset();
                          if (oldValue && linkTypeHasIssueTypeMap.get(oldValue.toString()) && issueTypeId) {
                            linkTypeHasIssueTypeMap.set(oldValue.toString(), (linkTypeHasIssueTypeMap.get(oldValue.toString()) as string[]).filter((item: string) => item !== issueTypeId));
                          }
                          linkIssueLinkageDataSet.current?.init(typeName, undefined);
                          linkIssueLinkageDataSet.current?.init(statusName, undefined);
                          }}
                        >
                          {(linkTypes || []).map((type: ILinkType) => (
                            <Option value={type.linkTypeId}>
                              {type.linkName}
                            </Option>
                          ))}
                        </Select>
                      </Col>
                      <Col span={7}>
                        <Select
                          label="工作项类型"
                          name={typeName}
                          disabled={!linkTypeId}
                          onChange={(value, oldValue) => {
                            if (value && linkTypeHasIssueTypeMap.get(linkTypeId.toString())) {
                              (linkTypeHasIssueTypeMap.get(linkTypeId.toString()) as string[]).push(value);
                            }
                            if (oldValue && linkTypeHasIssueTypeMap.get(linkTypeId.toString())) {
                              linkTypeHasIssueTypeMap.set(linkTypeId.toString(), (linkTypeHasIssueTypeMap.get(linkTypeId.toString()) as string[]).filter((item: string) => item !== oldValue));
                            }

                          getField(linkIssueLinkageDataSet, statusName)?.reset();
                          linkIssueLinkageDataSet.current?.init(statusName, undefined);
                          }}
                        >
                          {issueTypes?.filter((type: IIssueType) => ['story', 'task', 'bug'].includes(type.typeCode)).filter((issueType: IIssueType) => issueType.id === issueTypeId || !(linkTypeId ? linkTypeHasIssueTypeMap.get(linkTypeId.toString()) as string[] : []).includes(issueType.id)).map((type: IIssueType) => (
                            <Option value={type.id}>
                              {type.name}
                            </Option>
                          ))}
                        </Select>
                      </Col>
                      <Col span={7} key={id}>
                        <SelectStatus
                          label="指定状态"
                          key={`${key}-${selectedType}-${issueTypeId}`}
                          name={statusName}
                          disabled={!issueTypeId}
                          request={selectedType && issueTypeId ? () => statusTransformApi.getLinkageStatus({
                            issueTypeId: selectedType,
                            linkIssueTypeId: issueTypeId,
                            linkTypeId,
                            statusId: record.get('id'),
                          }) : () => {}}
                        // @ts-ignore
                          extraStatus={extraStatus ? [extraStatus] : undefined}
                        />
                      </Col>
                      <Col span={2}>
                        <Icon
                          type="delete_sweep-o"
                          style={{
                            color: 'var(--primary-color)',
                            cursor: 'pointer',
                          }}
                          onClick={() => {
                            LinkField.remove(key);
                            if (linkTypeId && issueTypeId) {
                              linkTypeHasIssueTypeMap.set(linkTypeId.toString(), (linkTypeHasIssueTypeMap.get(linkTypeId.toString()) as string[]).filter((item: string) => item !== issueTypeId));
                            }
                            removeField(linkIssueLinkageDataSet, linkTypeName);
                            removeField(linkIssueLinkageDataSet, typeName);
                            removeField(linkIssueLinkageDataSet, statusName);
                            removeField(linkIssueLinkageDataSet, triggeredName);
                          }}
                        />
                      </Col>
                    </Row>
                    <Row style={{ marginTop: -13 }}>
                      <Col span={24}>
                        <CheckBox name={triggeredName}>
                          {`允许其他联动配置触发${selectedTypeName}状态变更到${record.get('name')}时，执行此状态联动。`}
                        </CheckBox>
                      </Col>
                    </Row>
                  </>
                );
              })}
              <div>
                <Button
                  style={{ marginTop: -4 }}
                  icon="playlist_add"
                  onClick={() => {
                    const newKey = LinkField.add();
                    linkIssueAddFieldRule(newKey);
                  }}
                >
                  添加联动
                </Button>
              </div>
            </Form>
          )
        }
      </div>

    </div>
  );
};

export default observer(Linkage);
