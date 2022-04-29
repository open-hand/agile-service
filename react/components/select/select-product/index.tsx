import React from 'react';
import { has, mount } from '@choerodon/inject';
import { SELECT_PRODUCT } from '@/constants/AGILEPRO_INJECT';

const SelectProduct = (props: any) => {
  if (has(SELECT_PRODUCT)) {
    return mount(SELECT_PRODUCT, props);
  }
  return <></>;
};

export default SelectProduct;
