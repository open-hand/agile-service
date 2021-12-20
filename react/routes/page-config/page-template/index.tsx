import React from 'react';
import { IField, IModalProps } from '@/common/types';
import PageTemplate from './PageTemplate';
import PageTemplateProvider from './stores';
import { IPageTemplateStoreIssueType } from './stores/PageTemplateStore';
import { IAgileBaseSearchFieldInstance } from '@/components/field-pro/layouts/search';
import { IPageCascadeRuleModalField } from '../components/setting-linkage/Linkage';

export interface IInjectCascadeRuleConfigData {
  /** 检查是否可以级联，
   * @param currentIssueType 当前问题类型
   * @param field 字段信息
   * @returns 返回 null
   *  */
  checkPermissionLinkage?: (currentIssueType: IPageTemplateStoreIssueType, field: any) => boolean | null
  /**
   * 空页面时跳转页面
   * @returns 是否允许后续
   */
  emptyLinkToPageField?: (field: IPageCascadeRuleModalField, modal: IModalProps, defaultLinkToField: () => void) => void
  /**
   * 获取系统类字段的异步方法
   */
  getOptionsConfig?: (field: IPageCascadeRuleModalField, defaultRequest: () => Promise<any>, search?: string, page?: number) => Promise<{ content: Array<{ id: string, value: string }>, number: number, totalPage: number } | Array<{ id: string, value: string }>>
  /**
   * 获取字段实例
   */
  getFieldInstance?: IAgileBaseSearchFieldInstance['fieldInstance']
  /**
   * 获取当前字段所级联的字段配置项
   * @returns hidden 是否为隐藏字段 required 是否设为必填 当为 属性为 undefined 则使用原有隐藏/必填 逻辑
   */
  getFieldCascadeFieldConfig?: (field: IPageCascadeRuleModalField, cascadeField: IPageCascadeRuleModalField) => { hidden?: boolean, required?: boolean }
}
export const InjectCascadeRuleConfig = {} as Record<'backlog' | 'feature', IInjectCascadeRuleConfigData>;
/**
 * 注入级联配置相关内容
 * @param typeCode
 * @param config
 */
export function injectCascadeRuleConfig(typeCode: 'backlog' | 'feature', config: IInjectCascadeRuleConfigData) {
  InjectCascadeRuleConfig[typeCode] = config;
}

export default function Index(props: any) {
  return (
    <PageTemplateProvider {...props}>
      <PageTemplate />
    </PageTemplateProvider>
  );
}
