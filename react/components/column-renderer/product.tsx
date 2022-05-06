import React from 'react';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import ProductTags, { IProductItem } from '@/components/tag/product-tags';

const renderProduct = ({ record, productVOList }: { record?: Record, productVOList?: IProductItem[] }) => (
  <ProductTags data={productVOList || record?.get('productVOList')} />
);
export default renderProduct;
