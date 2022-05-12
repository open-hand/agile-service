import React, { forwardRef } from 'react';
import { has, mount } from '@choerodon/inject';
import { SELECT_PRODUCT } from '@/constants/AGILEPRO_INJECT';

const SelectProduct = forwardRef((props: any, ref: any) => {
  if (has(SELECT_PRODUCT)) {
    return mount(SELECT_PRODUCT, { ...props, ref });
  }
  return <></>;
});

export default SelectProduct;
