import React from 'react';
import { has, mount } from '@choerodon/inject';
import {
  SELECT_CATEGORY, SELECT_INFLUENCE, SELECT_PROBABILITY, SELECT_PROXIMITY,
} from '@/constants/AGILEPRO_INJECT';

interface Props {
  code: 'riskCategory' | 'riskInfluence' | 'riskProbability' | 'riskProximity'
}

const COMPONENT_CODE = {
  riskCategory: SELECT_CATEGORY,
  riskInfluence: SELECT_INFLUENCE,
  riskProbability: SELECT_PROBABILITY,
  riskProximity: SELECT_PROXIMITY,
};

const SelectRisk = ({ code, ...other }: Props) => {
  const componentCode = COMPONENT_CODE[code];
  if (componentCode && has(componentCode)) {
    return mount(componentCode, { onlyEnabled: true, ...other });
  }
  return <></>;
};

const SelectRiskCategory = (props: any) => <SelectRisk code="riskCategory" {...props} />;
const SelectRiskInfluence = (props: any) => <SelectRisk code="riskInfluence" {...props} />;
const SelectRiskProbability = (props: any) => <SelectRisk code="riskProbability" {...props} />;
const SelectRiskProximity = (props: any) => <SelectRisk code="riskProximity" {...props} />;

export {
  SelectRiskCategory, SelectRiskInfluence, SelectRiskProbability, SelectRiskProximity
};
