# Harmonization Validation Triage

Generated: 2026-02-11 07:41:14.181927

## Summary by Likely Cause

- raw_out_of_range_or_missing_codes: 92
- transform_mismatch_or_data_anomaly: 26
- range_spec_mismatch: 6
- transform_mismatch_or_data_anomaly; one_to_many_mapping: 6
- coverage_loss; one_to_many_mapping: 3
- monotonic_scale_mismatch: 2

## Issue Details

| spec | var_id | wave | source_var | status | error_checks | likely_cause | suggestion |
|---|---|---|---|---|---|---|---|
| clientelism | community_leader_contact | w5 | q72 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| clientelism | community_leader_contact | w6 | q70 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| clientelism | social_support_available | w3 | q29 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| clientelism | social_support_available | w4 | q30 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| clientelism | social_support_available | w5 | q31 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| corruption | corrupt_local_govt | w6 | q115 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| corruption | corrupt_national_govt | w6 | q116 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| democracy_satisfaction | hh_income_sat | w5 | SE14A | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| democracy | dem_vs_equality | w3 | q127 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| democracy | dem_vs_equality | w4 | q128 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| democracy | dem_vs_equality | w5 | q135 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| democracy | dem_vs_equality | w6 | q127 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| demographics | age | w3 | se3a | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | age | w5 | SE3_1 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | age | w6 | se3_1 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | current_status_unemployed | w3 | se9e | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | education_years | w2 | se5a | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | education_years | w3 | se5a | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | education_years | w4 | se5a | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | education_years | w5 | SE5A | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | education_years | w6 | se5a | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | idnumber | w1 | idnumber | warn | range | range_spec_mismatch | Check YAML valid_range or wave-specific exceptions. |
| demographics | idnumber | w2 | idnumber | warn | range | range_spec_mismatch | Check YAML valid_range or wave-specific exceptions. |
| demographics | idnumber | w3 | idnumber | warn | range | range_spec_mismatch | Check YAML valid_range or wave-specific exceptions. |
| demographics | idnumber | w4 | idnumber | warn | range | range_spec_mismatch | Check YAML valid_range or wave-specific exceptions. |
| demographics | idnumber | w5 | IDnumber | warn | range | range_spec_mismatch | Check YAML valid_range or wave-specific exceptions. |
| demographics | idnumber | w6 | idnumber | warn | range | range_spec_mismatch | Check YAML valid_range or wave-specific exceptions. |
| demographics | int_month | w3 | ir9 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | int_year | w3 | ir9 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | religion | w5 | SE6 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | religion | w6 | se6 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | religiosity_practice | w3 | se7 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | religiosity_self | w3 | se7a | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | seeking_employment | w3 | se9d | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | seeking_employment | w4 | se9d | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | seeking_employment | w5 | SE9D | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| demographics | seeking_employment | w6 | se9d | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| economic_attitudes | econ_family_income_fair | w5 | q163 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| globalization | glob_cultural_defense | w1 | q142 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| globalization | glob_cultural_defense | w2 | q144 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| globalization | glob_cultural_defense | w3 | q151 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| globalization | glob_cultural_defense | w4 | q151 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| globalization | glob_trade_protection | w2 | q146 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| globalization | glob_trade_protection | w3 | q152 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| globalization | glob_trade_protection | w4 | q152 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| governance | gov_economic_equality | w1 | q106 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| governance | gov_elections_real_choice | w5 | q119 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| governance | gov_free_to_organize | w1 | q113 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| governance | problem_most_important | w3 | q96 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| governance | problem_most_important | w4 | q99 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| governance | problem_most_important | w5 | q106 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| governance | problem_most_important | w6 | q97 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| institutional_trust | trust_civil_service | w5 | q12 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| institutional_trust | trust_courts | w5 | q8 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| institutional_trust | trust_election_commission | w5 | q16 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| institutional_trust | trust_election_commission | w6 | q18 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| institutional_trust | trust_local_government | w5 | q15 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| institutional_trust | trust_military | w5 | q13 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| institutional_trust | trust_national_government | w5 | q9 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| institutional_trust | trust_newspapers | w5 | q54 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| institutional_trust | trust_ngos | w5 | q17 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| institutional_trust | trust_ngos | w6 | q19 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| institutional_trust | trust_parliament | w5 | q11 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| institutional_trust | trust_police | w5 | q14 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| institutional_trust | trust_political_parties | w5 | q10 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| institutional_trust | trust_president | w5 | q7 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| institutional_trust | trust_television | w5 | q53 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| international_relations | intl_china_country_influence_valence_w4 | w4 | q169 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| international_relations | intl_china_world_influence | w5 | q178 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| international_relations | intl_close_to_asean | w6 | q186 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| international_relations | intl_development_model | w3 | q159 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| international_relations | intl_development_model | w4 | q167 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| international_relations | intl_development_model | w5 | q180 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| international_relations | intl_follow_foreign_events | w4 | q150 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| international_relations | intl_future_influence_asia | w3 | q158 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| international_relations | intl_future_influence_asia | w4 | q166 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| international_relations | intl_future_influence_asia | w5 | q179 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| international_relations | intl_most_influence_asia | w3 | q156 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| international_relations | intl_most_influence_asia | w4 | q163 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| international_relations | intl_most_influence_asia | w5 | q174 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| international_relations | intl_usa_country_influence_valence_w4 | w4 | q171 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| international_relations | intl_usa_world_influence | w5 | q176 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| meaning_of_democracy | dem_essential_core | w2 | q92 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| meaning_of_democracy | dem_meaning_set4 | w6 | q88 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| meaning_of_democracy | electoral_status | w2 | q39a | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| meaning_of_democracy | electoral_status | w3 | q33a | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| meaning_of_democracy | electoral_status | w4 | q34a | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| meaning_of_democracy | electoral_status | w5 | q34a | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| meaning_of_democracy | electoral_status | w6 | q34a | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| meaning_of_democracy | procedural_preference_index | w3 | q85 | error | coverage, crosstab | coverage_loss; one_to_many_mapping | Check recode logic for dropped values. Check recode function for non-deterministic mapping. |
| meaning_of_democracy | procedural_preference_index | w4 | q88 | error | coverage, crosstab | coverage_loss; one_to_many_mapping | Check recode logic for dropped values. Check recode function for non-deterministic mapping. |
| meaning_of_democracy | procedural_preference_index | w6 | q85 | error | coverage, crosstab | coverage_loss; one_to_many_mapping | Check recode logic for dropped values. Check recode function for non-deterministic mapping. |
| political_action | action_internet_political | w4 | q52 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| political_action | vote_buying_offered | w3 | q119 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| political_action | vote_buying_offered | w4 | q120 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| political_action | vote_buying_offered | w6 | q79 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| political_action | voted_last_election | w3 | q32 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| political_action | voted_last_election | w5 | q33 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| political_action | voted_last_election | w6 | q33 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| political_action | voted_winning_losing | w5 | q34a | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| political_action | voted_winning_losing | w6 | q34a | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| political_action | voting_frequency | w3 | q73 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| political_attitudes | citizen_loyalty_country | w2 | q153 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| political_attitudes | govt_withholds_info | w5 | q117 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| political_attitudes | govt_withholds_info | w6 | q108 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| political_attitudes | pol_discuss | w1 | q023 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| political_attitudes | pol_news_newspaper | w1 | q058 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| political_attitudes | pol_news_radio | w1 | q060 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| political_attitudes | pol_news_television | w1 | q059 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| public_services | access_healthcare | w2 | q46 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| public_services | access_healthcare | w3 | q40 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| public_services | access_healthcare | w4 | q41 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| public_services | access_identity_document | w4 | q39 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| public_services | access_internet | w5 | q44 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| public_services | access_police | w2 | q47 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| public_services | access_police | w3 | q41 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| public_services | access_police | w4 | q42 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| public_services | access_police | w5 | q43 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| public_services | access_public_school | w4 | q40 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| public_services | access_transport | w5 | q41 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| social_media | internet_political_info | w4 | q51 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| social_media | sm_express_political | w4 | q52 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| social_trust | trust_acquaintances | w2 | q26 | error | transformation, crosstab | transform_mismatch_or_data_anomaly; one_to_many_mapping | Inspect raw labels/value distributions for anomalies. Check recode function for non-deterministic mapping. |
| social_trust | trust_acquaintances | w3 | q27 | error | transformation, crosstab | transform_mismatch_or_data_anomaly; one_to_many_mapping | Inspect raw labels/value distributions for anomalies. Check recode function for non-deterministic mapping. |
| social_trust | trust_acquaintances | w5 | q26 | error | transformation | monotonic_scale_mismatch | Scale conversion may be off; recheck recode function. |
| social_trust | trust_generalized_binary | w1 | q024 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| social_trust | trust_generalized_binary | w3 | q23 | warn | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| social_trust | trust_generalized_binary | w5 | q22 | error | coverage | raw_out_of_range_or_missing_codes | Inspect raw codes; add to missing_conventions or update valid_range. |
| social_trust | trust_neighbors | w2 | q25 | error | transformation, crosstab | transform_mismatch_or_data_anomaly; one_to_many_mapping | Inspect raw labels/value distributions for anomalies. Check recode function for non-deterministic mapping. |
| social_trust | trust_neighbors | w3 | q26 | error | transformation, crosstab | transform_mismatch_or_data_anomaly; one_to_many_mapping | Inspect raw labels/value distributions for anomalies. Check recode function for non-deterministic mapping. |
| social_trust | trust_neighbors | w5 | q25 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| social_trust | trust_relatives | w2 | q24 | error | transformation, crosstab | transform_mismatch_or_data_anomaly; one_to_many_mapping | Inspect raw labels/value distributions for anomalies. Check recode function for non-deterministic mapping. |
| social_trust | trust_relatives | w3 | q25 | error | transformation, crosstab | transform_mismatch_or_data_anomaly; one_to_many_mapping | Inspect raw labels/value distributions for anomalies. Check recode function for non-deterministic mapping. |
| social_trust | trust_relatives | w5 | q24 | error | transformation | transform_mismatch_or_data_anomaly | Inspect raw labels/value distributions for anomalies. |
| social_trust | trust_strangers | w5 | q27 | error | transformation | monotonic_scale_mismatch | Scale conversion may be off; recheck recode function. |
